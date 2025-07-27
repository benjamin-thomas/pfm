module Server.Main (main, startServer) where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Int (fromString)
import Data.Traversable (traverse)
import Foreign.Object as FO
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import HTTPurple
  ( class Generic
  , Method(..)
  , Request
  , ResponseM
  , RouteDuplex'
  , ServerM
  , header
  , int
  , methodNotAllowed
  , mkRoute
  , noArgs
  , ok
  , response
  , segment
  , serve
  , toString
  , (/)
  )
import HTTPurple.Response (Response)
import SQLite3 (DBConnection)
import Server.Database as DB
import Server.DB.Account as Account
import Server.DB.Budgets.Queries as BudgetQueries
import Server.DB.Category as Category
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.LedgerView (LedgerViewFilters)
import Server.DB.Transactions.Queries as TransactionQueries
import Server.Types.TransactionWrite (TransactionWrite(..))
import Yoga.JSON as JSON

-- Server configuration types
data Environment = Dev | Test

data Route
  = Home
  | Categories
  | CategoryById Int
  | Accounts
  | AccountBalances
  | AccountLedger Int
  | Budgets
  | BudgetById Int
  | Transactions
  | TransactionById Int

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Users": "users" / noArgs
  , "UserById": "users" / int segment
  , "Categories": "categories" / noArgs
  , "CategoryById": "categories" / int segment
  , "Accounts": "accounts" / noArgs
  , "AccountBalances": "accounts" / "balances" / noArgs
  , "AccountLedger": "accounts" / int segment / "ledger"
  , "Budgets": "budgets" / noArgs
  , "BudgetById": "budgets" / int segment
  , "Transactions": "transactions" / noArgs
  , "TransactionById": "transactions" / int segment
  }

main :: Effect Unit
main = do
  log "[SERVER] Booting up..."

  -- Require mandatory APP_ENV environment variable
  appEnv <- lookupEnv "APP_ENV"
  case appEnv of
    Nothing -> throw "APP_ENV environment variable is required (dev|test)"
    Just "dev" -> do
      log "[SERVER] Starting in development mode"
      startAppWithConfig { port: 8081, dbPath: "./db.sqlite", shouldResetSchema: false }
    Just "test" -> do
      log "[SERVER] Starting in e2e-test mode"
      startAppWithConfig { port: 8082, dbPath: "./db.e2e-test.sqlite", shouldResetSchema: true }
    Just other -> throw $ "Invalid APP_ENV: " <> other <> " (must be dev|test)"

startAppWithConfig :: { port :: Int, dbPath :: String, shouldResetSchema :: Boolean } -> Effect Unit
startAppWithConfig { port, dbPath, shouldResetSchema } = do
  log $ "[SERVER] Using database: " <> dbPath
  log $ "[SERVER] Starting server on port: " <> show port
  DB.initDatabase dbPath # runAff_
    case _ of
      Left err -> log $ "Failed to initialize database: " <> show err
      Right db -> do
        log "Database initialized successfully"
        if shouldResetSchema then do
          log "[SERVER] Resetting database schema and seeding with fixtures (test mode)"
          DB.seedFromOfx "test/OfxParser/fixture.ofx" db # runAff_
            case _ of
              Left seedErr -> log $ "Failed to seed database: " <> show seedErr
              Right _ -> do
                log "Database schema ready and seeded with test fixtures"
                void $ startServer port db
        else do
          log "[SERVER] Using existing database schema (dev mode)"
          void $ startServer port db

startServer :: Int -> DBConnection -> ServerM
startServer port db =
  serve { port } { route, router }
  where
  router :: Request Route -> ResponseM
  router =
    corsMiddleware
      $ jsonMiddleware
      $ makeRouter db

corsMiddleware :: (Request Route -> ResponseM) -> Request Route -> ResponseM
corsMiddleware route' request = do
  response <- route' request
  pure $ (response :: Response)
    { headers = foldl (<>) response.headers
        [ header "Access-Control-Allow-Origin" "*"
        , header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
        , header "Access-Control-Allow-Headers" "Content-Type, Authorization"
        ]
    }

jsonMiddleware :: (Request Route -> ResponseM) -> Request Route -> ResponseM
jsonMiddleware route' request = do
  response <- route' request
  pure $ response { headers = header "Content-Type" "application/json" <> response.headers }

makeRouter :: DBConnection -> Request Route -> ResponseM
makeRouter db req =
  case req.route of
    Home ->
      case req.method of
        Get -> ok "PFM PureScript Server is running!"
        Options -> ok ""
        _ -> methodNotAllowed

    Categories ->
      case req.method of
        Get -> do
          categories <- Category.getAllCategories db
          ok $ JSON.writeJSON categories
        Options -> ok ""
        _ -> methodNotAllowed

    CategoryById categoryId ->
      case req.method of
        Get -> do
          maybeCategory <- Category.getCategoryById categoryId db
          case maybeCategory of
            Just category -> ok $ JSON.writeJSON category
            Nothing -> response 404 $ JSON.writeJSON { error: "Category not found" }
        Options -> ok ""
        _ -> methodNotAllowed

    Accounts ->
      case req.method of
        Get -> do
          accounts <- Account.getAllAccounts db
          ok $ JSON.writeJSON accounts
        Options -> ok ""
        _ -> methodNotAllowed

    AccountBalances ->
      case req.method of
        Get -> do
          let accountIdsParam = FO.lookup "accountIds" req.query
          case accountIdsParam of
            Nothing -> response 400 $ JSON.writeJSON { error: "Missing accountIds parameter" }
            Just idsStr -> do
              let
                idStrings = split (Pattern ",") idsStr
                parsedIds = traverse fromString idStrings
              case parsedIds of
                Nothing -> response 400 $ JSON.writeJSON { error: "Invalid accountIds parameter: must be comma-separated integers" }
                Just accountIds -> do
                  balances <- Account.getAccountBalances accountIds db
                  ok $ JSON.writeJSON balances
        Options -> ok ""
        _ -> methodNotAllowed

    AccountLedger accountId ->
      case req.method of
        Get -> do
          let filters = parseFilters req.query
          ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO accountId filters db
          ok $ JSON.writeJSON ledgerRows
        Options -> ok ""
        _ -> methodNotAllowed

    Budgets ->
      case req.method of
        Get -> do
          budgets <- BudgetQueries.getAllBudgets db
          ok $ JSON.writeJSON budgets
        Options -> ok ""
        _ -> methodNotAllowed

    BudgetById budgetId ->
      case req.method of
        Get -> do
          maybeBudget <- BudgetQueries.getBudgetById budgetId db
          case maybeBudget of
            Just budget -> ok $ JSON.writeJSON budget
            Nothing -> response 404 $ JSON.writeJSON { error: "Budget not found" }
        Options -> ok ""
        _ -> methodNotAllowed

    Transactions ->
      case req.method of
        Get -> do
          transactions <- TransactionQueries.getAllTransactions db
          ok $ JSON.writeJSON transactions
        Post -> do
          -- Parse request body
          bodyStr <- toString req.body
          case JSON.readJSON bodyStr of
            Left err -> response 400 $ JSON.writeJSON { error: "Invalid JSON: " <> show err }
            Right (TransactionWrite txWrite) -> do
              -- Get or create budget ID for the transaction date
              maybeBudgetId <- BudgetQueries.getBudgetIdForDate txWrite.dateUnix db
              budgetId <- case maybeBudgetId of
                Nothing -> BudgetQueries.insertBudgetForDate txWrite.dateUnix db
                Just bid -> pure bid

              -- Create the transaction row
              let
                transactionRow = TransactionQueries.TransactionNewRow
                  { budgetId: Just budgetId
                  , fromAccountId: txWrite.fromAccountId
                  , toAccountId: txWrite.toAccountId
                  , uniqueFitId: Nothing
                  , dateUnix: txWrite.dateUnix
                  , descrOrig: txWrite.descr
                  , descr: txWrite.descr
                  , cents: txWrite.cents
                  }

              -- Insert the transaction
              TransactionQueries.insertTransaction transactionRow db
              response 201 ""
        Options -> ok ""
        _ -> methodNotAllowed

    TransactionById transactionId ->
      case req.method of
        Get -> do
          maybeTransaction <- TransactionQueries.getTransactionById transactionId db
          case maybeTransaction of
            Just transaction -> ok $ JSON.writeJSON transaction
            Nothing -> response 404 $ JSON.writeJSON { error: "Transaction not found" }
        Put -> do
          -- Parse request body
          bodyStr <- toString req.body
          case JSON.readJSON bodyStr of
            Left err -> response 400 $ JSON.writeJSON { error: "Invalid JSON: " <> show err }
            Right (TransactionWrite txWrite) -> do
              -- Create the transaction row (budget ID not needed for update)
              let
                transactionRow = TransactionQueries.TransactionNewRow
                  { budgetId: Nothing
                  , fromAccountId: txWrite.fromAccountId
                  , toAccountId: txWrite.toAccountId
                  , uniqueFitId: Nothing
                  , dateUnix: txWrite.dateUnix
                  , descrOrig: txWrite.descr
                  , descr: txWrite.descr
                  , cents: txWrite.cents
                  }

              -- Update the transaction
              TransactionQueries.updateTransaction transactionId transactionRow db
              response 204 ""
        Delete -> do
          TransactionQueries.deleteTransaction transactionId db
          ok "Transaction deleted"
        Options -> ok ""
        _ -> methodNotAllowed

-- Helper functions for query parameter parsing

-- | Parse filter parameters from query object
parseFilters :: FO.Object String -> LedgerViewFilters
parseFilters query =
  { description: FO.lookup "description" query
  , soundexDescr: FO.lookup "soundexDescr" query
  , minAmount: FO.lookup "minAmount" query >>= fromString
  , maxAmount: FO.lookup "maxAmount" query >>= fromString
  , filterUnknownExpenses: (_ == "1") <$> FO.lookup "filterUnknownExpenses" query
  }
