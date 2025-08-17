module Server.DB.Transactions.Queries
  ( TransactionDB(..)
  , TransactionNewRow(..)
  , getAllTransactionsDB
  , getTransactionByIdDB
  , getAllTransactions
  , getTransactionById
  , insertTransaction
  , insertTransactionsBulk
  , updateTransaction
  , deleteTransaction
  , deleteAllTransactions
  ) where

import Prelude

import Data.Array (head, length, intercalate, concatMap)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Foreign (Foreign)
import Yoga.JSON as JSON
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DateFormat (formatUnixTimestamp)
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import Shared.Types (Transaction(..))
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names
newtype TransactionRow = MkTransactionRow
  { transaction_id :: Int
  , budget_id :: Int
  , from_account_id :: Int
  , to_account_id :: Int
  , unique_fit_id :: Maybe String
  , date :: Int
  , descr_orig :: String
  , descr :: String
  , cents :: Int
  , created_at :: Int
  , updated_at :: Int
  }

derive instance Generic TransactionRow _
derive newtype instance ReadForeign TransactionRow

-- | Domain type with proper PureScript naming
newtype TransactionDB = TransactionDB
  { transactionId :: Int
  , budgetId :: Int
  , fromAccountId :: Int
  , toAccountId :: Int
  , uniqueFitId :: Maybe String
  , dateUnix :: Int -- Unix timestamp
  , descrOrig :: String
  , descr :: String
  , cents :: Int -- Amount in cents
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Generic TransactionDB _
derive newtype instance Show TransactionDB
derive newtype instance Eq TransactionDB
derive newtype instance ReadForeign TransactionDB
derive newtype instance WriteForeign TransactionDB

-- | Convert database row to domain type
rowToTransaction :: TransactionRow -> TransactionDB
rowToTransaction (MkTransactionRow row) = TransactionDB
  { transactionId: row.transaction_id
  , budgetId: row.budget_id
  , fromAccountId: row.from_account_id
  , toAccountId: row.to_account_id
  , uniqueFitId: row.unique_fit_id
  , dateUnix: row.date
  , descrOrig: row.descr_orig
  , descr: row.descr
  , cents: row.cents
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

newtype TransactionNewRow = TransactionNewRow
  { budgetId :: Maybe Int
  , fromAccountId :: Int
  , toAccountId :: Int
  , uniqueFitId :: Maybe String
  , dateUnix :: Int
  , descrOrig :: String
  , descr :: String
  , cents :: Int
  }

derive newtype instance Show TransactionNewRow

-- | Get all transactions (returns raw DB data)
getAllTransactionsDB :: SQLite3.DBConnection -> Aff (Array TransactionDB)
getAllTransactionsDB db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/getAllTransactions.sql"
  rows <- SQLite3.queryDB db sql []
  fromDbRows "transactions" rowToTransaction rows

-- | Get transaction by ID (returns raw DB data)
getTransactionByIdDB :: Int -> SQLite3.DBConnection -> Aff (Maybe TransactionDB)
getTransactionByIdDB transactionId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/getTransactionById.sql"
  rows <- SQLite3.queryDB db sql [ JSON.writeImpl transactionId ]
  transactions <- fromDbRows "transactions" rowToTransaction rows
  pure $ head transactions

-- | Insert a transaction into the database
insertTransaction :: TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
insertTransaction (TransactionNewRow txn) db = do
  liftEffect $ log $ "Inserting transaction: " <> show txn.dateUnix <> " " <> txn.descr
  case txn.budgetId of
    Nothing -> do
      liftEffect $ throwException $ error "Budget ID is required for transaction insertion"
    Just budgetId -> do
      sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/insertTransaction.sql"
      _ <- SQLite3.queryDB db sql
        [ JSON.writeImpl budgetId
        , JSON.writeImpl txn.fromAccountId
        , JSON.writeImpl txn.toAccountId
        , JSON.writeImpl txn.uniqueFitId
        , JSON.writeImpl txn.dateUnix
        , JSON.writeImpl txn.descrOrig
        , JSON.writeImpl txn.descr
        , JSON.writeImpl txn.cents
        ]
      pure unit

-- | Insert multiple transactions in a single SQL statement (much more efficient)
insertTransactionsBulk :: Array TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
insertTransactionsBulk transactions db = do
  liftEffect $ log $ "Bulk inserting " <> show (length transactions) <> " transactions"

  if length transactions == 0 then
    pure unit
  else do
    -- Build the SQL with multiple VALUES clauses
    let
      valuesClause = intercalate ", " $ map (\_ -> "(?, ?, ?, ?, ?, ?, ?, ?)") transactions
      sql =
        """
        INSERT INTO transactions
          ( budget_id
          , from_account_id
          , to_account_id
          , unique_fit_id
          , date
          , descr_orig
          , descr
          , cents
          )
        VALUES """ <> valuesClause

      -- Flatten all parameters into a single array
      params = concatMap transactionToParams transactions

    _ <- SQLite3.queryDB db sql params
    pure unit

  where
  transactionToParams :: TransactionNewRow -> Array Foreign
  transactionToParams (TransactionNewRow txn) = case txn.budgetId of
    Nothing -> [] -- This should never happen as we validate before calling bulk insert
    Just budgetId ->
      [ JSON.writeImpl budgetId
      , JSON.writeImpl txn.fromAccountId
      , JSON.writeImpl txn.toAccountId
      , JSON.writeImpl txn.uniqueFitId
      , JSON.writeImpl txn.dateUnix
      , JSON.writeImpl txn.descrOrig
      , JSON.writeImpl txn.descr
      , JSON.writeImpl txn.cents
      ]

-- | Update a transaction
updateTransaction :: Int -> TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
updateTransaction transactionId (TransactionNewRow txn) db = do
  liftEffect $ log $ "Updating transaction: " <> show transactionId
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/updateTransaction.sql"
  _ <- SQLite3.queryDB db sql
    [ JSON.writeImpl txn.fromAccountId
    , JSON.writeImpl txn.toAccountId
    , JSON.writeImpl txn.dateUnix
    , JSON.writeImpl txn.descr
    , JSON.writeImpl txn.cents
    , JSON.writeImpl transactionId
    ]
  pure unit

-- | Delete a transaction
deleteTransaction :: Int -> SQLite3.DBConnection -> Aff Unit
deleteTransaction transactionId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/deleteTransaction.sql"
  _ <- SQLite3.queryDB db sql [ JSON.writeImpl transactionId ]
  pure unit

-- | Delete all transactions from the database
deleteAllTransactions :: SQLite3.DBConnection -> Aff Unit
deleteAllTransactions db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/deleteAllTransactions.sql"
  _ <- SQLite3.queryDB db sql []
  pure unit

-- | Convert database transaction to DTO
dbTransactionToTransaction :: TransactionDB -> Transaction
dbTransactionToTransaction (TransactionDB dbTx) =
  Transaction
    { id: dbTx.transactionId
    , budgetId: dbTx.budgetId
    , fromAccountId: dbTx.fromAccountId
    , fromAccountName: getAccountName dbTx.fromAccountId
    , toAccountId: dbTx.toAccountId
    , toAccountName: getAccountName dbTx.toAccountId
    , uniqueFitId: dbTx.uniqueFitId
    , date: formatUnixTimestamp dbTx.dateUnix
    , description: dbTx.descr
    , amount: centsToNumber dbTx.cents
    }
  where
  centsToNumber :: Int -> Number
  centsToNumber cents = (toNumber cents) / 100.0

  -- Simple lookup for account names - in a real app this would come from the API
  getAccountName :: Int -> String
  getAccountName accountId = case accountId of
    1 -> "OpeningBalance"
    2 -> "Checking account"
    3 -> "Savings account"
    4 -> "Unknown_INCOME"
    5 -> "Employer"
    6 -> "Unknown_EXPENSE"
    7 -> "Groceries"
    8 -> "Communications"
    9 -> "Transport"
    10 -> "Health"
    11 -> "Energy"
    12 -> "Clothing"
    13 -> "Leisure"
    _ -> "Unknown Account"

-- | Transaction operations (with DTO conversion)
getAllTransactions :: SQLite3.DBConnection -> Aff (Array Transaction)
getAllTransactions db = do
  dbTransactions <- getAllTransactionsDB db
  pure $ map dbTransactionToTransaction dbTransactions

getTransactionById :: Int -> SQLite3.DBConnection -> Aff (Maybe Transaction)
getTransactionById transactionId db = do
  maybeDbTransaction <- getTransactionByIdDB transactionId db
  pure $ map dbTransactionToTransaction maybeDbTransaction