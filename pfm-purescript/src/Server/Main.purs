module Server.Main (main, startServer, AppEnv(..)) where

import Prelude hiding ((/))

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, runAff_, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Effect.Exception (throw, try)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object as FO
import HTTPurple (class Generic, ClosingHandler(..), Method(..), Request, ResponseM, RouteDuplex', ServerM, header, int, methodNotAllowed, mkRoute, noArgs, ok, response, response', segment, serve, toString, (/))
import HTTPurple.Headers (empty)
import HTTPurple.Response (Response)
import Node.Encoding (Encoding(..))
import Node.Process (lookupEnv, mkSignalH)
import Node.Process as Process
import Data.Posix.Signal (Signal(..))
import Node.EventEmitter as EE
import Node.Stream as Stream
import SQLite3 (DBConnection, closeDB)
import Server.DB.Account as Account
import Server.DB.Budgets.Queries as BudgetQueries
import Server.DB.Category as Category
import Server.DB.LedgerView (LedgerViewFilters)
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Suggestions.Queries as SuggestionQueries
import Server.DB.Transactions.Queries as TransactionQueries
import Server.Database as DB
import Server.Types.TransactionWrite (TransactionWrite(..))
import Shared.Types (SSE_Event(..))
import Yoga.JSON as JSON

-- | Create SSE data line from an SSE_Event
sseDataLine :: SSE_Event -> String
sseDataLine event = "data: " <> stringify (encodeJson event) <> "\n\n"

newtype SSE_Client = SSE_Client Stream.Duplex

-- | Broadcast an SSE event to all connected clients
-- Removes disconnected clients (failed writes) from the ref
broadcastSSE :: Ref (Array SSE_Client) -> SSE_Event -> Effect Unit
broadcastSSE sseClientsRef event = do
  sseClients <- Ref.read sseClientsRef
  let message = sseDataLine event
  validClients <- Array.filterA (\client -> tryWrite client message) sseClients
  Ref.write validClients sseClientsRef
  log $ "[SSE] Broadcasted to " <> show (Array.length validClients) <> " clients"

  where
  -- Try to write to stream, return true if successful (client still connected)
  -- FIXME: I thought I could detect disconnected clients that way, but it doesn't seem to work!
  tryWrite :: SSE_Client -> String -> Effect Boolean
  tryWrite (SSE_Client stream) message = do
    result <- try $ Stream.writeString stream UTF8 message
    case result of
      Right _ -> pure true
      Left _ -> pure false

data AppEnv
  = DevEnv
  | TestEnv

derive instance Eq AppEnv
derive instance Ord AppEnv

instance Bounded AppEnv where
  bottom = DevEnv
  top = TestEnv

instance Show AppEnv where
  show DevEnv = "dev"
  show TestEnv = "test"

allAppEnvs :: Array String
allAppEnvs = show <$> ([ bottom, top ] :: Array AppEnv)

parseAppEnv :: Maybe String -> Effect AppEnv
parseAppEnv = case _ of
  Just "dev" -> pure DevEnv
  Just "test" -> pure TestEnv
  _ -> throw $ "Invalid or missing APP_ENV, must be one of: " <> show allAppEnvs

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
  | TransactionSuggestions
  | TestEnvResetDb
  | Events
  | RequestClientReload

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Categories": "categories" / noArgs
  , "CategoryById": "categories" / int segment
  , "Accounts": "accounts" / noArgs
  , "AccountBalances": "accounts" / "balances" / noArgs
  , "AccountLedger": "accounts" / int segment / "ledger"
  , "Budgets": "budgets" / noArgs
  , "BudgetById": "budgets" / int segment
  , "Transactions": "transactions" / noArgs
  , "TransactionById": "transactions" / int segment
  , "TransactionSuggestions": "transactions" / "suggestions" / noArgs
  , "TestEnvResetDb": "test" / "reset-db" / noArgs
  , "Events": "events" / noArgs
  , "RequestClientReload": "request-client-reload" / noArgs
  }

main :: Effect Unit
main = do
  log "[SERVER] Booting up..."

  -- Require mandatory APP_ENV environment variable
  appEnvStrMay <- lookupEnv "APP_ENV"
  appEnv <- parseAppEnv appEnvStrMay
  case appEnv of
    DevEnv -> do
      log "[SERVER] Starting in development mode"
      startAppWithConfig appEnv { port: 8081, dbPath: "./db.sqlite" }
    TestEnv -> do
      log "[SERVER] Starting in e2e-test mode"
      startAppWithConfig appEnv { port: 8082, dbPath: "./db.e2e-test.sqlite" }

startAppWithConfig :: AppEnv -> { port :: Int, dbPath :: String } -> Effect Unit
startAppWithConfig appEnv { port, dbPath } = do
  log $ "[SERVER] Starting up with init args: " <> show { appEnv, port, dbPath }

  -- Do all async work first, then start the server synchronously at the end
  launchAff_ do
    -- Initialize database
    db <- DB.initDatabase dbPath
    liftEffect $ log "Database initialized successfully"

    -- Set up our own signal handlers with database cleanup immediately after DB init
    let
      makeSignalHandler :: Signal -> Effect Unit
      makeSignalHandler signal = do
        let header = "[" <> show signal <> "] "
        Console.log $ header <> "Received " <> show signal <> ", shutting down..."
        closeDB db # runAff_
          case _ of
            Left err -> do
              Console.error $ header <> "Failed to close database: " <> show err
              Process.exit' 1
            Right _ -> do
              Console.log $ header <> "Database closed successfully"
              Process.exit' 0

    liftEffect $ Process.process # EE.on_ (mkSignalH SIGINT) (makeSignalHandler SIGINT)
    liftEffect $ Process.process # EE.on_ (mkSignalH SIGTERM) (makeSignalHandler SIGTERM)

    -- Handle app environment specific setup
    case appEnv of
      TestEnv -> do
        liftEffect $ log "[SERVER] Resetting database schema and seeding with fixtures (test mode)"
        DB.seedFromOfx "test/OfxParser/fixture.ofx" db
        liftEffect $ log "Database schema ready and seeded with test fixtures"
      DevEnv -> do
        liftEffect $ log "[SERVER] Using existing database schema (dev mode)"

    -- Now start the server with the fully initialized database
    void $ liftEffect $ startServer appEnv port db

startServer :: AppEnv -> Int -> DBConnection -> ServerM
startServer appEnv port db = do
  -- Create ref to track active SSE connections (shared across all requests)
  sseClientsRef <- liftEffect $ Ref.new []
  let
    router :: Request Route -> ResponseM
    router =
      corsMiddleware
        $ jsonMiddleware
        $ makeRouter appEnv db sseClientsRef
  serve { port, closingHandler: Just NoClosingHandler } { route, router }

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

makeRouter :: AppEnv -> DBConnection -> Ref (Array SSE_Client) -> Request Route -> ResponseM
makeRouter appEnv db sseClientsRef req =
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

    TransactionSuggestions ->
      case req.method of
        Get -> do
          let fromAccountIdParam = FO.lookup "fromAccountId" req.query
          let toAccountIdParam = FO.lookup "toAccountId" req.query
          case fromAccountIdParam, toAccountIdParam of
            Just fromIdStr, Just toIdStr -> do
              case fromString fromIdStr, fromString toIdStr of
                Just fromAccountId, Just toAccountId -> do
                  suggestions <- SuggestionQueries.getSuggestions fromAccountId toAccountId db
                  ok $ JSON.writeJSON suggestions
                _, _ -> response 400 $ JSON.writeJSON { error: "Invalid account IDs" }
            _, _ -> response 400 $ JSON.writeJSON { error: "Missing fromAccountId or toAccountId parameter" }
        Post -> do
          -- Apply batch suggestions
          bodyStr <- toString req.body
          case JSON.readJSON bodyStr of
            Left err -> response 400 $ JSON.writeJSON { error: "Invalid JSON: " <> show err }
            Right (suggestions :: Array SuggestionQueries.SuggestionInsert) -> do
              SuggestionQueries.batchApplySuggestions suggestions db
              ok ""
        Options -> ok ""
        _ -> methodNotAllowed

    TestEnvResetDb ->
      case appEnv, req.method of
        TestEnv, Post -> do
          -- Reset database by reseeding from OFX fixture
          liftEffect $ log "[SERVER] Test environment database reset requested"
          DB.seedFromOfx "test/OfxParser/fixture.ofx" db
          ok $ JSON.writeJSON { message: "Database reset successful" }
        TestEnv, Options -> ok ""
        TestEnv, _ -> methodNotAllowed
        DevEnv, _ -> response 404 $ JSON.writeJSON { error: "Endpoint only available in test environment" }

    Events ->
      case req.method of
        Get -> do
          -- curl -N http://localhost:8081/events
          liftEffect $ log "[SSE] Client connected (pure PureScript)"

          -- Create a PassThrough stream using node-streams
          stream <- liftEffect Stream.newPassThrough

          -- Add stream to the ref for broadcasting
          liftEffect $ do
            sseClients <- Ref.read sseClientsRef
            let newSseClients = Array.snoc sseClients (SSE_Client stream)
            Ref.write newSseClients sseClientsRef
            log $ "[SSE] Added client to broadcast list. Total clients: " <> show (Array.length newSseClients)

          -- Send initial connection message using shared SSE_Event type
          _ <- liftEffect
            $ Stream.writeString stream UTF8
            $ sseDataLine
            $ SSE_Connected { message: "Pure PureScript SSE established" }

          -- Fork a process to send periodic pings
          _ <- forkAff $ pingLoop stream

          -- Send SSE headers
          let
            sseHeaders = empty
              <> header "Content-Type" "text/event-stream"
              <> header "Cache-Control" "no-cache"
              <> header "Connection" "keep-alive"
              <> header "Access-Control-Allow-Origin" "*"

          -- Return the stream as the response body (Duplex can be used as Readable)
          response' 200 sseHeaders stream

          where
          pingLoop :: Stream.Duplex -> Aff Unit
          pingLoop stream = do
            delay (Milliseconds 10_000.0)
            _ <- liftEffect $ do
              instant <- now
              let Milliseconds unixTimeMs = unInstant instant
              Stream.writeString stream UTF8
                $ sseDataLine
                $ SSE_Ping { unixTimeMs }
            pingLoop stream
        Options -> ok ""
        _ -> methodNotAllowed

    RequestClientReload ->
      case req.method of
        Post -> do
          liftEffect $ log "[RELOAD] Received client reload request"
          liftEffect $ broadcastSSE sseClientsRef SSE_ClientShouldRefreshData
          ok "Client reload request received"
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
