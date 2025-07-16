module Server.DB.Transaction
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
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throwException, error)
import Server.DB.Utils (fromDbRows)
import Yoga.JSON as JSON
import SQLite3 as SQLite3
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
rowToTransactionDB :: TransactionRow -> TransactionDB
rowToTransactionDB (MkTransactionRow row) = TransactionDB
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

-- | Get all transactions (returns raw DB data)
getAllTransactionsDB :: SQLite3.DBConnection -> Aff (Array TransactionDB)
getAllTransactionsDB db = do
  rows <- SQLite3.queryDB db
    "SELECT transaction_id, budget_id, from_account_id, to_account_id, unique_fit_id, date, descr_orig, descr, cents, created_at, updated_at FROM transactions ORDER BY date DESC"
    []
  fromDbRows "transactions" rowToTransactionDB rows

-- | Get transaction by ID (returns raw DB data)
getTransactionByIdDB :: Int -> SQLite3.DBConnection -> Aff (Maybe TransactionDB)
getTransactionByIdDB transactionId db = do
  rows <- SQLite3.queryDB db
    "SELECT transaction_id, budget_id, from_account_id, to_account_id, unique_fit_id, date, descr_orig, descr, cents, created_at, updated_at FROM transactions WHERE transaction_id = ?"
    [ JSON.writeImpl transactionId ]
  transactions <- fromDbRows "transactions" rowToTransactionDB rows
  pure $ head transactions

-- | Insert a transaction into the database
insertTransaction :: TransactionNewRow -> SQLite3.DBConnection -> Aff Unit
insertTransaction (TransactionNewRow txn) db = do
  liftEffect $ log $ "Inserting transaction: " <> show txn.dateUnix <> " " <> txn.descr
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
  _ <- SQLite3.queryDB db "DELETE FROM transactions WHERE transaction_id = ?" [ JSON.writeImpl transactionId ]
  pure unit

-- | Delete all transactions from the database
deleteAllTransactions :: SQLite3.DBConnection -> Aff Unit
deleteAllTransactions db = do
  _ <- SQLite3.queryDB db "DELETE FROM transactions" []
  pure unit