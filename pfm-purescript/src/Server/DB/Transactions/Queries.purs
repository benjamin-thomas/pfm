module Server.DB.Transactions.Queries
  ( TransactionDB(..)
  , TransactionNewRow(..)
  , getAllTransactionsDB
  , getTransactionByIdDB
  , insertTransaction
  , updateTransaction
  , deleteTransaction
  , deleteAllTransactions
  ) where

import Prelude

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names
newtype TransactionRow = TransactionRow
  { transaction_id :: Int
  , budget_id :: Int
  , from_account_id :: Int
  , to_account_id :: Int
  , unique_fit_id :: Foreign -- Handle NULL values
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
rowToTransaction (TransactionRow row) =
  let
    -- Handle NULL unique_fit_id safely
    uniqueFitId = case unsafeFromForeign row.unique_fit_id of
      "" -> Nothing
      s -> Just s
  in
    TransactionDB
      { transactionId: row.transaction_id
      , budgetId: row.budget_id
      , fromAccountId: row.from_account_id
      , toAccountId: row.to_account_id
      , uniqueFitId: uniqueFitId
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
  rows <- SQLite3.queryDB db sql [unsafeToForeign transactionId]
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
        [ unsafeToForeign budgetId
        , unsafeToForeign txn.fromAccountId
        , unsafeToForeign txn.toAccountId
        , unsafeToForeign txn.uniqueFitId
        , unsafeToForeign txn.dateUnix
        , unsafeToForeign txn.descrOrig
        , unsafeToForeign txn.descr
        , unsafeToForeign txn.cents
        ]
      pure unit

-- | Update a transaction
updateTransaction :: Int -> TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
updateTransaction transactionId (TransactionNewRow txn) db = do
  liftEffect $ log $ "Updating transaction: " <> show transactionId
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/updateTransaction.sql"
  _ <- SQLite3.queryDB db sql
    [ unsafeToForeign txn.fromAccountId
    , unsafeToForeign txn.toAccountId
    , unsafeToForeign txn.dateUnix
    , unsafeToForeign txn.descr
    , unsafeToForeign txn.cents
    , unsafeToForeign transactionId
    ]
  pure unit

-- | Delete a transaction
deleteTransaction :: Int -> SQLite3.DBConnection -> Aff Unit
deleteTransaction transactionId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/deleteTransaction.sql"
  _ <- SQLite3.queryDB db sql [unsafeToForeign transactionId]
  pure unit

-- | Delete all transactions from the database
deleteAllTransactions :: SQLite3.DBConnection -> Aff Unit
deleteAllTransactions db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/sql/deleteAllTransactions.sql"
  _ <- SQLite3.queryDB db sql []
  pure unit