module Server.Database
  ( createSchema
  , deleteUser
  , getAllTransactions
  , getAllUsers
  , getLedgerViewRows
  , getTransactionById
  , getUserById
  , initDatabase
  , insertUser
  , seedDatabase
  , seedFromOfx
  ) where

import Prelude

import Data.Array (head, reverse, length, uncons)
import Data.DateTime (DateTime(..))
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.Time (Time(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import SQLite3 as SQLite3
import Server.Conversion (dbLedgerViewRowToLedgerViewRow, dbTransactionToTransaction)
import Server.DB.Budgets.Queries as Budget
import Server.DB.LedgerView as LedgerView
import Server.DB.Transactions.Queries as Transaction
import Server.OfxParser (StatementTransaction, TimeStamp(..), parseOfx)
import Shared.Types (LedgerViewRow, Transaction, User(..))

-- | Initialize the database and create tables
initDatabase :: String -> Aff SQLite3.DBConnection
initDatabase dbPath = do
  liftEffect $ log $ "Initializing database at: " <> dbPath
  db <- SQLite3.newDB dbPath
  createTables db
  pure db

-- | Create the users table
createTables :: SQLite3.DBConnection -> Aff Unit
createTables db = do
  liftEffect $ log "Creating tables..."
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE IF NOT EXISTS users
      ( id INTEGER PRIMARY KEY
      , firstName TEXT NOT NULL
      , lastName TEXT NOT NULL
      , UNIQUE (firstName, lastName)
      )
  """
    []
  liftEffect $ log "Tables created successfully"

-- | Create proper database tables from SQL schema
createSchema :: SQLite3.DBConnection -> Aff Unit
createSchema db = do
  liftEffect $ log "Creating proper database schema..."
  -- For now, let's just create the essential tables needed for the wip function
  _ <- SQLite3.queryDB db "DROP TABLE IF EXISTS transactions" []
  _ <- SQLite3.queryDB db "DROP TABLE IF EXISTS budgets" []
  _ <- SQLite3.queryDB db "DROP TABLE IF EXISTS accounts" []
  _ <- SQLite3.queryDB db "DROP TABLE IF EXISTS categories" []

  -- Create categories table
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE categories
      ( category_id INTEGER PRIMARY KEY
      , name        TEXT    NOT NULL UNIQUE CHECK (TRIM(name) <> '')
      , created_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , updated_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      )
    """
    []

  -- Create accounts table
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE accounts
      ( account_id  INTEGER PRIMARY KEY
      , category_id INTEGER NOT NULL REFERENCES categories(category_id)
      , name        TEXT    NOT NULL UNIQUE CHECK (TRIM(name) <> '')
      , created_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , updated_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      )
    """
    []

  -- Create budgets table
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE budgets
      ( budget_id  INTEGER PRIMARY KEY
      , starts_on INTEGER NOT NULL
      , ends_on   INTEGER NOT NULL
      , created_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , updated_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , CHECK (starts_on >= 0 AND starts_on < ends_on)
      )
    """
    []

  -- Create transactions table
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE transactions
      ( transaction_id  INTEGER        PRIMARY KEY
      , budget_id       INTEGER        NOT NULL REFERENCES budgets(budget_id)
      , from_account_id INTEGER        NOT NULL REFERENCES accounts(account_id)
      , to_account_id   INTEGER        NOT NULL REFERENCES accounts(account_id)
      , unique_fit_id   TEXT           NULL
      , date            INTEGER        NOT NULL
      , descr_orig      TEXT           NOT NULL
      , descr           TEXT           NOT NULL
      , cents           INTEGER        NOT NULL CHECK (cents > 0)
      , created_at      INTEGER        NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , updated_at      INTEGER        NOT NULL DEFAULT (strftime('%s', current_timestamp))
      , CHECK (from_account_id <> to_account_id)
      )
    """
    []

  -- Insert initial data
  _ <- SQLite3.queryDB db
    """
    INSERT INTO categories (name) VALUES 
      ('Equity'), ('Assets'), ('Income'), ('Expenses')
    """
    []

  _ <- SQLite3.queryDB db
    """
    INSERT INTO accounts (category_id, name) VALUES 
      (1, 'OpeningBalance'),
      (2, 'Checking account'),
      (2, 'Savings account'),
      (3, 'Unknown_INCOME'),
      (3, 'Employer'),
      (4, 'Unknown_EXPENSE'),
      (4, 'Groceries'),
      (4, 'Communications'),
      (4, 'Transport'),
      (4, 'Health'),
      (4, 'Energy'),
      (4, 'Clothing'),
      (4, 'Leisure')
    """
    []

  liftEffect $ log "Database schema created successfully"

getUserById :: Int -> SQLite3.DBConnection -> Aff (Maybe User)
getUserById userId db = do
  rows <- SQLite3.queryDB db "SELECT id, firstName, lastName FROM users WHERE id = ?" [ unsafeToForeign userId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { id :: Int, firstName :: String, lastName :: String }
      pure $ Just $ User { id: Just obj.id, firstName: obj.firstName, lastName: obj.lastName }

-- | Get all users from the database
getAllUsers :: SQLite3.DBConnection -> Aff (Array User)
getAllUsers db = do
  rows <- SQLite3.queryDB db "SELECT id, firstName, lastName FROM users" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToUser rowArray
  where
  rowToUser :: Foreign -> User
  rowToUser row =
    let
      obj = unsafeFromForeign row :: { id :: Int, firstName :: String, lastName :: String }
    in
      User { id: Just obj.id, firstName: obj.firstName, lastName: obj.lastName }

-- | Insert a new user into the database
insertUser :: User -> SQLite3.DBConnection -> Aff User
insertUser (User user) db = do
  _ <- SQLite3.queryDB db
    "INSERT INTO users (firstName, lastName) VALUES (?, ?)"
    [ unsafeToForeign user.firstName, unsafeToForeign user.lastName ]

  -- Get the newly created user
  rows <- SQLite3.queryDB db
    "SELECT id, firstName, lastName FROM users WHERE firstName = ? AND lastName = ? ORDER BY id DESC LIMIT 1"
    [ unsafeToForeign user.firstName, unsafeToForeign user.lastName ]

  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure $ User user -- fallback
    Just row ->
      let
        obj = unsafeFromForeign row :: { id :: Int, firstName :: String, lastName :: String }
      in
        pure $ User { id: Just obj.id, firstName: obj.firstName, lastName: obj.lastName }

-- | Delete a user by ID
deleteUser :: Int -> SQLite3.DBConnection -> Aff Unit
deleteUser userId db = do
  _ <- SQLite3.queryDB db "DELETE FROM users WHERE id = ?" [ unsafeToForeign userId ]
  pure unit

-- | Convert TimeStamp to Unix timestamp
timestampToUnix :: TimeStamp -> Int
timestampToUnix = case _ of
  ShortDate date ->
    let
      midnight = unsafePartial $ case toEnum 0, toEnum 0, toEnum 0, toEnum 0 of
        Just h, Just m, Just s, Just ms -> Time h m s ms
      result = floor $ unwrap $ unInstant $ fromDateTime $ DateTime date midnight
    in
      -- Convert from milliseconds to seconds 
      result / 1000
  FullDate dateTime ->
    let
      result = floor $ unwrap $ unInstant $ fromDateTime dateTime
    in
      result / 1000

-- | Convert StatementTransaction to TransactionNewRow
fromOfxTransaction :: String -> StatementTransaction -> Transaction.TransactionNewRow
fromOfxTransaction accountNumber tx =
  let
    cents = floor $ Decimal.toNumber tx.amount * 100.0

    -- Hard-coded account IDs (matching Haskell version)
    checkingAccountId = 2
    unknownIncomeAccountId = 4
    unknownExpenseAccountId = 6

    isNegative = tx.amount < Decimal.fromInt 0
    isPositive = tx.amount > Decimal.fromInt 0

    fromAccountId = if isNegative then checkingAccountId else unknownIncomeAccountId
    toAccountId = if isPositive then checkingAccountId else unknownExpenseAccountId

    uniqueFitId = Just $ accountNumber <> ":" <> tx.fitId
  in
    Transaction.TransactionNewRow
      { budgetId: Nothing -- Will be set when inserting
      , fromAccountId
      , toAccountId
      , uniqueFitId
      , dateUnix: timestampToUnix tx.posted
      , descrOrig: trim tx.name
      , descr: trim tx.name
      , cents: if cents < 0 then (-cents) else cents
      }

-- | WIP function to seed database from OFX file
seedFromOfx :: String -> SQLite3.DBConnection -> Aff Unit
seedFromOfx ofxFilePath db = do
  liftEffect $ log "=== Resetting the database ==="
  createSchema db

  liftEffect $ log "=== Reading OFX file ==="
  ofxContent <- FS.readTextFile UTF8 ofxFilePath

  case parseOfx ofxContent of
    Left err -> do
      liftEffect $ log $ "OFX parsing failed: " <> err
      liftEffect $ throwException $ error $ "OFX parsing failed: " <> err
    Right batch -> do
      liftEffect $ log $ "=== Processing " <> show (length batch.transactions) <> " transactions ==="

      -- Clear existing transactions
      Transaction.deleteAllTransactions db

      -- Process each transaction
      let reversedTransactions = reverse batch.transactions -- to have budget ids in logical order
      processTransactions db batch.accountNumber reversedTransactions

      liftEffect $ log "=== Transactions inserted ==="
  where
  processTransactions :: SQLite3.DBConnection -> String -> Array StatementTransaction -> Aff Unit
  processTransactions dbConn accountNumber txns = do
    case uncons txns of
      Nothing -> pure unit
      Just { head: tx, tail: rest } -> do
        let txnRow = fromOfxTransaction accountNumber tx
        let (Transaction.TransactionNewRow txnData) = txnRow

        -- Get or create budget for this transaction date
        maybeBudgetId <- Budget.getBudgetIdForDate txnData.dateUnix dbConn
        budgetId <- case maybeBudgetId of
          Just bid -> pure bid
          Nothing -> Budget.insertBudgetForDate txnData.dateUnix dbConn

        -- Insert the transaction with the budget ID
        let txnWithBudget = Transaction.TransactionNewRow $ txnData { budgetId = Just budgetId }
        liftEffect $ log $ "Budget ID: " <> show budgetId <> " for transaction"
        Transaction.insertTransaction txnWithBudget dbConn

        -- Process remaining transactions
        processTransactions dbConn accountNumber rest

-- | Seed the database with initial data
seedDatabase :: SQLite3.DBConnection -> Aff Unit
seedDatabase db = do
  liftEffect $ log "Checking if database needs seeding..."
  rows <- SQLite3.queryDB db "SELECT COUNT(*) as count FROM users" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> seed
    Just row ->
      let
        obj = unsafeFromForeign row :: { count :: Int }
      in
        if obj.count == 0 then seed else liftEffect $ log "Database already has data, skipping seed"
  where
  seed = do
    liftEffect $ log "Seeding database..."
    _ <- insertUser (User { id: Nothing, firstName: "John", lastName: "Doe" }) db
    _ <- insertUser (User { id: Nothing, firstName: "Jane", lastName: "Smith" }) db
    liftEffect $ log "Database seeded successfully"

-- | Transaction operations (with DTO conversion)
getAllTransactions :: SQLite3.DBConnection -> Aff (Array Transaction)
getAllTransactions db = do
  dbTransactions <- Transaction.getAllTransactionsDB db
  pure $ map dbTransactionToTransaction dbTransactions

getTransactionById :: Int -> SQLite3.DBConnection -> Aff (Maybe Transaction)
getTransactionById transactionId db = do
  maybeDbTransaction <- Transaction.getTransactionByIdDB transactionId db
  pure $ map dbTransactionToTransaction maybeDbTransaction

-- | Get ledger view rows for a specific account (with DTO conversion)
getLedgerViewRows :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRow)
getLedgerViewRows accountId db = do
  dbRows <- LedgerView.getLedgerViewRows accountId db
  pure $ map dbLedgerViewRowToLedgerViewRow dbRows

