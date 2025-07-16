module Server.Main (main, startServer) where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
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
  , (/)
  )
import HTTPurple.Response (Response)
import SQLite3 (DBConnection)
import Server.Database as DB
import Server.DB.Account as Account
import Server.DB.Budgets.Queries as BudgetQueries
import Server.DB.Category as Category
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Transactions.Queries as TransactionQueries
import Yoga.JSON as JSON

data Route
  = Home
  | Categories
  | CategoryById Int
  | Accounts
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
  , "AccountLedger": "accounts" / int segment / "ledger"
  , "Budgets": "budgets" / noArgs
  , "BudgetById": "budgets" / int segment
  , "Transactions": "transactions" / noArgs
  , "TransactionById": "transactions" / int segment
  }

main :: Effect Unit
main = do
  log "[SERVER] Booting up..."
  DB.initDatabase "./db.sqlite" # runAff_
    case _ of
      Left err -> log $ "Failed to initialize database: " <> show err
      Right db -> do
        log "Database initialized successfully"
        void $ startServer 8080 db

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
makeRouter db { route: route', method } =
  case route' of
    Home ->
      case method of
        Get -> ok "PFM PureScript Server is running!"
        _ -> methodNotAllowed

    Categories ->
      case method of
        Get -> do
          categories <- Category.getAllCategories db
          ok $ JSON.writeJSON categories
        _ -> methodNotAllowed

    CategoryById categoryId ->
      case method of
        Get -> do
          maybeCategory <- Category.getCategoryById categoryId db
          case maybeCategory of
            Just category -> ok $ JSON.writeJSON category
            Nothing -> response 404 $ JSON.writeJSON { error: "Category not found" }
        _ -> methodNotAllowed

    Accounts ->
      case method of
        Get -> do
          accounts <- Account.getAllAccounts db
          ok $ JSON.writeJSON accounts
        _ -> methodNotAllowed

    AccountLedger accountId ->
      case method of
        Get -> do
          ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO accountId db
          ok $ JSON.writeJSON ledgerRows
        _ -> methodNotAllowed

    Budgets ->
      case method of
        Get -> do
          budgets <- BudgetQueries.getAllBudgets db
          ok $ JSON.writeJSON budgets
        _ -> methodNotAllowed

    BudgetById budgetId ->
      case method of
        Get -> do
          maybeBudget <- BudgetQueries.getBudgetById budgetId db
          case maybeBudget of
            Just budget -> ok $ JSON.writeJSON budget
            Nothing -> response 404 $ JSON.writeJSON { error: "Budget not found" }
        _ -> methodNotAllowed

    Transactions ->
      case method of
        Get -> do
          transactions <- TransactionQueries.getAllTransactions db
          ok $ JSON.writeJSON transactions
        _ -> methodNotAllowed

    TransactionById transactionId ->
      case method of
        Get -> do
          maybeTransaction <- TransactionQueries.getTransactionById transactionId db
          case maybeTransaction of
            Just transaction -> ok $ JSON.writeJSON transaction
            Nothing -> response 404 $ JSON.writeJSON { error: "Transaction not found" }
        Delete -> do
          TransactionQueries.deleteTransaction transactionId db
          ok "Transaction deleted"
        _ -> methodNotAllowed