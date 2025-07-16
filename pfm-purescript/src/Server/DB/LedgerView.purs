module Server.DB.LedgerView
  ( LedgerViewRowDB(..)
  , getLedgerViewRows
  ) where

import Prelude

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

-- | Database row type matching SQL column names
newtype LedgerViewRow = MkLedgerViewRow
  { transaction_id :: Int
  , budget_id :: Int
  , from_account_id :: Int
  , from_account_name :: String
  , to_account_id :: Int
  , to_account_name :: String
  , date_unix :: Int
  , date :: String -- ISO date from SQLite
  , descr :: String
  , flow_cents :: Int -- Positive for money coming in, negative for going out
  , running_balance_cents :: Int -- Running balance in cents
  , created_at_unix :: Int
  , updated_at_unix :: Int
  }

derive instance Generic LedgerViewRow _
derive newtype instance ReadForeign LedgerViewRow

-- | Domain type with proper PureScript naming
newtype LedgerViewRowDB = LedgerViewRowDB
  { transactionId :: Int
  , budgetId :: Int
  , fromAccountId :: Int
  , fromAccountName :: String
  , toAccountId :: Int
  , toAccountName :: String
  , dateUnix :: Int
  , date :: String -- ISO date from SQLite
  , descr :: String
  , flowCents :: Int -- Positive for money coming in, negative for going out
  , runningBalanceCents :: Int -- Running balance in cents
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Eq LedgerViewRowDB
instance Show LedgerViewRowDB where
  show (LedgerViewRowDB row) = 
    "LedgerViewRowDB { transactionId: " <> show row.transactionId <> 
    ", descr: \"" <> row.descr <> "\"" <>
    ", flowCents: " <> show row.flowCents <>
    ", runningBalanceCents: " <> show row.runningBalanceCents <> " }"

-- | Convert database row to domain type
rowToLedgerViewRowDB :: LedgerViewRow -> LedgerViewRowDB
rowToLedgerViewRowDB (MkLedgerViewRow row) = LedgerViewRowDB
  { transactionId: row.transaction_id
  , budgetId: row.budget_id
  , fromAccountId: row.from_account_id
  , fromAccountName: row.from_account_name
  , toAccountId: row.to_account_id
  , toAccountName: row.to_account_name
  , dateUnix: row.date_unix
  , date: row.date
  , descr: row.descr
  , flowCents: row.flow_cents
  , runningBalanceCents: row.running_balance_cents
  , createdAtUnix: row.created_at_unix
  , updatedAtUnix: row.updated_at_unix
  }

-- | Get ledger view rows for a specific account ID
getLedgerViewRows :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRowDB)
getLedgerViewRows accountId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/LedgerView/sql/getLedgerViewRows.sql"
  rows <- SQLite3.queryDB db sql [ JSON.writeImpl accountId, JSON.writeImpl accountId, JSON.writeImpl accountId ]
  fromDbRows "ledger_view" rowToLedgerViewRowDB rows