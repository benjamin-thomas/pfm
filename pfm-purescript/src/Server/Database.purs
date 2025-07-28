module Server.Database
  ( createSchema
  , dateToUnixMs
  , dateToUnixS
  , initDatabase
  , seedFromOfx
  ) where

import Prelude

import Data.Array (length, reverse, uncons)
import Data.Date (Date)
import Data.DateTime.Instant (Instant, fromDate, unInstant)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Debug (spy)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafeCrashWith)
import SQLite3 as SQLite3
import Server.DB.Budgets.Queries as BudgetQueries
import Server.DB.Transactions.Queries as TransactionQueries
import Server.OfxParser (StatementTransaction, TimeStamp(..), parseOfx)

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

{-

> toDateTime <$> instant (Milliseconds 1752611105509.0)
(Just (DateTime (Date (Year 2025) July (Day 15)) (Time (Hour 20) (Minute 25) (Second 5) (Millisecond 509))))



 -}
dateToUnixMs :: Date -> Number
dateToUnixMs date =
  let
    inst :: Instant
    inst = fromDate date

    milliseconds :: Number
    milliseconds = unwrap (unInstant inst)
  in
    milliseconds

dateToUnixS :: Date -> Int
dateToUnixS date = Int.floor $ dateToUnixMs date / 1000.0

-- | Convert TimeStamp to Unix timestamp
timestampToUnixMs :: TimeStamp -> Int
timestampToUnixMs ts = case spy "timestampToUnix" ts of
  ShortDate date ->
    let
      result = dateToUnixS date
    in
      spy "result (A)" result
  FullDate dateTime ->
    unsafeCrashWith "FullDate not implemented in timestampToUnixMs"

-- | Convert StatementTransaction to TransactionNewRow
fromOfxTransaction :: String -> StatementTransaction -> TransactionQueries.TransactionNewRow
fromOfxTransaction accountNumber tx =
  let
    cents = Int.round $ Decimal.toNumber tx.amount * 100.0

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
    TransactionQueries.TransactionNewRow
      { budgetId: Nothing -- Will be set when inserting
      , fromAccountId
      , toAccountId
      , uniqueFitId
      , dateUnix: timestampToUnixMs tx.posted
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
      TransactionQueries.deleteAllTransactions db

      -- Process each transaction
      let reversedTransactions = reverse batch.transactions -- to have budget ids in logical order
      processTransactions db batch.accountNumber reversedTransactions

      liftEffect $ log "=== Transactions inserted ==="
      liftEffect $ log "=== Categorizing some transactions for demo purposes ==="
      categorizeTransactionsForDemo db
      liftEffect $ log "=== Transaction categorization complete ==="
  where
  processTransactions :: SQLite3.DBConnection -> String -> Array StatementTransaction -> Aff Unit
  processTransactions dbConn accountNumber txns = do
    case uncons txns of
      Nothing -> pure unit
      Just { head: tx, tail: rest } -> do
        let txnRow = fromOfxTransaction accountNumber tx
        liftEffect $ log $ "==> FROM OFX TRANSACTION: " <> show txnRow
        let (TransactionQueries.TransactionNewRow txnData) = txnRow

        -- Get or create budget for this transaction date
        maybeBudgetId <- BudgetQueries.getBudgetIdForDate txnData.dateUnix dbConn
        budgetId <- case maybeBudgetId of
          Just bid -> pure bid
          Nothing -> BudgetQueries.insertBudgetForDate txnData.dateUnix dbConn

        -- Insert the transaction with the budget ID
        let txnWithBudget = TransactionQueries.TransactionNewRow $ txnData { budgetId = Just budgetId }
        liftEffect $ log $ "Budget ID: " <> show budgetId <> " for transaction"
        TransactionQueries.insertTransaction txnWithBudget dbConn

        -- Process remaining transactions
        processTransactions dbConn accountNumber rest

-- | Add one properly categorized grocery transaction for demo purposes
categorizeTransactionsForDemo :: SQLite3.DBConnection -> Aff Unit
categorizeTransactionsForDemo db = do
  -- Get or create a budget for the demo transaction date (slightly earlier than fixture data)
  let demoDateUnix = 1725062400 -- Aug 31, 2024
  maybeBudgetId <- BudgetQueries.getBudgetIdForDate demoDateUnix db
  budgetId <- case maybeBudgetId of
    Just bid -> pure bid
    Nothing -> BudgetQueries.insertBudgetForDate demoDateUnix db

  -- Insert a grocery transaction that goes to the Groceries account (account 7)
  let
    demoTransaction = TransactionQueries.TransactionNewRow
      { budgetId: Just budgetId
      , fromAccountId: 2 -- Checking account
      , toAccountId: 7 -- Groceries account
      , uniqueFitId: Nothing
      , dateUnix: demoDateUnix
      , descrOrig: "GROCERY SUPERMARKET"
      , descr: "GROCERY SUPERMARKET"
      , cents: 8500 -- 85.00 euros
      }

  TransactionQueries.insertTransaction demoTransaction db

  -- Add another uncategorized grocery transaction for batch suggestion testing
  let
    demoTransaction2 = TransactionQueries.TransactionNewRow
      { budgetId: Just budgetId
      , fromAccountId: 2 -- Checking account
      , toAccountId: 6   -- Unknown_EXPENSE account (uncategorized)
      , uniqueFitId: Nothing
      , dateUnix: demoDateUnix - 86400 -- One day earlier
      , descrOrig: "GROCERY STORE DOWNTOWN"
      , descr: "GROCERY STORE DOWNTOWN"
      , cents: 4250 -- 42.50 euros
      }

  TransactionQueries.insertTransaction demoTransaction2 db
  pure unit

