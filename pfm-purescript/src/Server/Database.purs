module Server.Database
  ( Account(..)
  , Budget(..)
  , Category(..)
  , Transaction(..)
  , TransactionNewRow(..)
  , createSchema
  , deleteAllTransactions
  , deleteTransaction
  , deleteUser
  , getAllAccounts
  , getAllBudgets
  , getAllCategories
  , getAllTransactions
  , getAllUsers
  , getBudgetById
  , getBudgetIdForDate
  , getCategoryById
  , getTransactionById
  , getUserById
  , initDatabase
  , insertBudgetForDate
  , insertTransaction
  , insertUser
  , seedDatabase
  , seedFromOfx
  , updateTransaction
  ) where

import Prelude

import Data.Array (head, reverse, length, uncons)
import Data.Date (Date)
import Data.DateTime (DateTime(..), date, time)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (trim)
import Data.Time (Time(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Enum (toEnum)
import Partial.Unsafe (unsafePartial)
import Data.Time.Component (Hour, Minute, Second, Millisecond)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Effect.Exception (throwException, error)
import SQLite3 as SQLite3
import Server.OfxParser (OfxBatch, StatementTransaction, TimeStamp(..), parseOfx)
import Shared.Types (User(..))

-- | Database type definitions
newtype Category = Category
  { categoryId :: Int
  , name :: String
  , createdAt :: Int
  , updatedAt :: Int
  }

derive newtype instance Show Category
derive newtype instance Eq Category

newtype Account = Account
  { accountId :: Int
  , categoryId :: Int
  , name :: String
  , createdAt :: Int
  , updatedAt :: Int
  }

derive newtype instance Show Account
derive newtype instance Eq Account

newtype Budget = Budget
  { budgetId :: Int
  , startsOn :: Int
  , endsOn :: Int
  , createdAt :: Int
  , updatedAt :: Int
  }

derive newtype instance Show Budget
derive newtype instance Eq Budget

newtype Transaction = Transaction
  { transactionId :: Int
  , budgetId :: Int
  , fromAccountId :: Int
  , toAccountId :: Int
  , uniqueFitId :: Maybe String
  , date :: Int
  , descrOrig :: String
  , descr :: String
  , cents :: Int
  , createdAt :: Int
  , updatedAt :: Int
  }

derive newtype instance Show Transaction
derive newtype instance Eq Transaction

newtype TransactionNewRow = TransactionNewRow
  { budgetId :: Maybe Int
  , fromAccountId :: Int
  , toAccountId :: Int
  , uniqueFitId :: Maybe String
  , date :: Int
  , descrOrig :: String
  , descr :: String
  , cents :: Int
  }

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

-- | Insert a transaction into the database
insertTransaction :: TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
insertTransaction (TransactionNewRow txn) db = do
  liftEffect $ log $ "Inserting transaction: " <> show txn.date <> " " <> txn.descr
  case txn.budgetId of
    Nothing -> do
      liftEffect $ throwException $ error "Budget ID is required for transaction insertion"
    Just budgetId -> do
      _ <- SQLite3.queryDB db
        """
        INSERT INTO transactions 
          (budget_id, from_account_id, to_account_id, unique_fit_id, date, descr_orig, descr, cents)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?)
        """
        [ unsafeToForeign budgetId
        , unsafeToForeign txn.fromAccountId
        , unsafeToForeign txn.toAccountId
        , unsafeToForeign txn.uniqueFitId
        , unsafeToForeign txn.date
        , unsafeToForeign txn.descrOrig
        , unsafeToForeign txn.descr
        , unsafeToForeign txn.cents
        ]
      pure unit

-- | Delete all transactions from the database
deleteAllTransactions :: SQLite3.DBConnection -> Aff Unit
deleteAllTransactions db = do
  _ <- SQLite3.queryDB db "DELETE FROM transactions" []
  pure unit

-- | Get budget ID for a date (returns Nothing if not found)
getBudgetIdForDate :: Int -> SQLite3.DBConnection -> Aff (Maybe Int)
getBudgetIdForDate dateUnix db = do
  rows <- SQLite3.queryDB db
    "SELECT budget_id FROM budgets WHERE ? >= starts_on AND ? < ends_on"
    [ unsafeToForeign dateUnix, unsafeToForeign dateUnix ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int }
      pure $ Just obj.budget_id

-- | Insert a budget for a date (monthly budget)
insertBudgetForDate :: Int -> SQLite3.DBConnection -> Aff Int
insertBudgetForDate dateUnix db = do
  -- Create a monthly budget period
  let startsOn = dateUnix
  let endsOn = dateUnix + (30 * 24 * 60 * 60) -- 30 days later

  liftEffect $ log $ "Creating budget: starts_on=" <> show startsOn <> ", ends_on=" <> show endsOn

  _ <- SQLite3.queryDB db
    "INSERT INTO budgets (starts_on, ends_on) VALUES (?, ?)"
    [ unsafeToForeign startsOn, unsafeToForeign endsOn ]

  -- Get the inserted budget ID
  rows <- SQLite3.queryDB db "SELECT last_insert_rowid() as budget_id" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure 1 -- fallback
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int }
      pure obj.budget_id

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
fromOfxTransaction :: String -> StatementTransaction -> TransactionNewRow
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
    TransactionNewRow
      { budgetId: Nothing -- Will be set when inserting
      , fromAccountId
      , toAccountId
      , uniqueFitId
      , date: timestampToUnix tx.posted
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
      deleteAllTransactions db

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
        let (TransactionNewRow txnData) = txnRow

        -- Get or create budget for this transaction date
        maybeBudgetId <- getBudgetIdForDate txnData.date dbConn
        budgetId <- case maybeBudgetId of
          Just bid -> pure bid
          Nothing -> insertBudgetForDate txnData.date dbConn

        -- Insert the transaction with the budget ID
        let txnWithBudget = TransactionNewRow $ txnData { budgetId = Just budgetId }
        liftEffect $ log $ "Budget ID: " <> show budgetId <> " for transaction"
        insertTransaction txnWithBudget dbConn

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

-- =============================================================================
-- CATEGORIES CRUD
-- =============================================================================

-- | Get all categories
getAllCategories :: SQLite3.DBConnection -> Aff (Array Category)
getAllCategories db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories ORDER BY category_id" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToCategory rowArray
  where
  rowToCategory :: Foreign -> Category
  rowToCategory row =
    let
      obj = unsafeFromForeign row :: { category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
    in
      Category { categoryId: obj.category_id, name: obj.name, createdAt: obj.created_at, updatedAt: obj.updated_at }

-- | Get category by ID
getCategoryById :: Int -> SQLite3.DBConnection -> Aff (Maybe Category)
getCategoryById categoryId db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories WHERE category_id = ?" [ unsafeToForeign categoryId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
      pure $ Just $ Category { categoryId: obj.category_id, name: obj.name, createdAt: obj.created_at, updatedAt: obj.updated_at }

-- =============================================================================
-- ACCOUNTS CRUD  
-- =============================================================================

-- | Get all accounts
getAllAccounts :: SQLite3.DBConnection -> Aff (Array Account)
getAllAccounts db = do
  rows <- SQLite3.queryDB db "SELECT account_id, category_id, name, created_at, updated_at FROM accounts ORDER BY category_id, name" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToAccount rowArray
  where
  rowToAccount :: Foreign -> Account
  rowToAccount row =
    let
      obj = unsafeFromForeign row :: { account_id :: Int, category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
    in
      Account { accountId: obj.account_id, categoryId: obj.category_id, name: obj.name, createdAt: obj.created_at, updatedAt: obj.updated_at }

-- =============================================================================
-- BUDGETS CRUD
-- =============================================================================

-- | Get all budgets
getAllBudgets :: SQLite3.DBConnection -> Aff (Array Budget)
getAllBudgets db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets ORDER BY starts_on DESC" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToBudget rowArray
  where
  rowToBudget :: Foreign -> Budget
  rowToBudget row =
    let
      obj = unsafeFromForeign row :: { budget_id :: Int, starts_on :: Int, ends_on :: Int, created_at :: Int, updated_at :: Int }
    in
      Budget { budgetId: obj.budget_id, startsOn: obj.starts_on, endsOn: obj.ends_on, createdAt: obj.created_at, updatedAt: obj.updated_at }

-- | Get budget by ID
getBudgetById :: Int -> SQLite3.DBConnection -> Aff (Maybe Budget)
getBudgetById budgetId db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets WHERE budget_id = ?" [ unsafeToForeign budgetId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int, starts_on :: Int, ends_on :: Int, created_at :: Int, updated_at :: Int }
      pure $ Just $ Budget { budgetId: obj.budget_id, startsOn: obj.starts_on, endsOn: obj.ends_on, createdAt: obj.created_at, updatedAt: obj.updated_at }

-- =============================================================================
-- TRANSACTIONS CRUD
-- =============================================================================

-- | Get all transactions
getAllTransactions :: SQLite3.DBConnection -> Aff (Array Transaction)
getAllTransactions db = do
  rows <- SQLite3.queryDB db 
    "SELECT transaction_id, budget_id, from_account_id, to_account_id, unique_fit_id, date, descr_orig, descr, cents, created_at, updated_at FROM transactions ORDER BY date DESC" 
    []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToTransaction rowArray
  where
  rowToTransaction :: Foreign -> Transaction
  rowToTransaction row =
    let
      obj = unsafeFromForeign row :: { transaction_id :: Int, budget_id :: Int, from_account_id :: Int, to_account_id :: Int, unique_fit_id :: Foreign, date :: Int, descr_orig :: String, descr :: String, cents :: Int, created_at :: Int, updated_at :: Int }
      -- Handle NULL unique_fit_id safely
      uniqueFitId = case unsafeFromForeign obj.unique_fit_id of
        "" -> Nothing
        s -> Just s
    in
      Transaction 
        { transactionId: obj.transaction_id
        , budgetId: obj.budget_id
        , fromAccountId: obj.from_account_id
        , toAccountId: obj.to_account_id
        , uniqueFitId: uniqueFitId
        , date: obj.date
        , descrOrig: obj.descr_orig
        , descr: obj.descr
        , cents: obj.cents
        , createdAt: obj.created_at
        , updatedAt: obj.updated_at
        }

-- | Get transaction by ID
getTransactionById :: Int -> SQLite3.DBConnection -> Aff (Maybe Transaction)
getTransactionById transactionId db = do
  rows <- SQLite3.queryDB db 
    "SELECT transaction_id, budget_id, from_account_id, to_account_id, unique_fit_id, date, descr_orig, descr, cents, created_at, updated_at FROM transactions WHERE transaction_id = ?" 
    [ unsafeToForeign transactionId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let 
        obj = unsafeFromForeign row :: { transaction_id :: Int, budget_id :: Int, from_account_id :: Int, to_account_id :: Int, unique_fit_id :: Foreign, date :: Int, descr_orig :: String, descr :: String, cents :: Int, created_at :: Int, updated_at :: Int }
        -- Handle NULL unique_fit_id safely
        uniqueFitId = case unsafeFromForeign obj.unique_fit_id of
          "" -> Nothing
          s -> Just s
      pure $ Just $ Transaction 
        { transactionId: obj.transaction_id
        , budgetId: obj.budget_id
        , fromAccountId: obj.from_account_id
        , toAccountId: obj.to_account_id
        , uniqueFitId: uniqueFitId
        , date: obj.date
        , descrOrig: obj.descr_orig
        , descr: obj.descr
        , cents: obj.cents
        , createdAt: obj.created_at
        , updatedAt: obj.updated_at
        }

-- | Update a transaction
updateTransaction :: Int -> TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
updateTransaction transactionId (TransactionNewRow txn) db = do
  liftEffect $ log $ "Updating transaction: " <> show transactionId
  _ <- SQLite3.queryDB db
    """
    UPDATE transactions 
    SET from_account_id = ?, to_account_id = ?, date = ?, descr = ?, cents = ?, updated_at = strftime('%s', 'now')
    WHERE transaction_id = ?
    """
    [ unsafeToForeign txn.fromAccountId
    , unsafeToForeign txn.toAccountId
    , unsafeToForeign txn.date
    , unsafeToForeign txn.descr
    , unsafeToForeign txn.cents
    , unsafeToForeign transactionId
    ]
  pure unit

-- | Delete a transaction
deleteTransaction :: Int -> SQLite3.DBConnection -> Aff Unit
deleteTransaction transactionId db = do
  _ <- SQLite3.queryDB db "DELETE FROM transactions WHERE transaction_id = ?" [ unsafeToForeign transactionId ]
  pure unit