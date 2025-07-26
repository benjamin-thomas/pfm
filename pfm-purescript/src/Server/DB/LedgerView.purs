module Server.DB.LedgerView
  ( LedgerViewRowDB(..)
  , LedgerViewFilters(..)
  , getLedgerViewRows
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toNullable)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import SQLite3 (queryObjectDB)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

-- | Filter parameters for ledger view queries
type LedgerViewFilters =
  { description :: Maybe String -- Filter by description (case insensitive LIKE)
  , soundex :: Maybe String -- Filter by soundex similarity  
  , minAmount :: Maybe Int -- Minimum amount in cents
  , maxAmount :: Maybe Int -- Maximum amount in cents
  , unknownExpensesOnly :: Maybe Boolean -- Show only unknown expenses
  }

-- | Query parameters for SQL named parameters
type LedgerViewQueryParams =
  { "$accountId" :: Int
  , "$descriptionFilter" :: Nullable String
  , "$minAmountCents" :: Nullable Int
  , "$maxAmountCents" :: Nullable Int
  , "$unknownExpensesOnly" :: Boolean
  }

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
  , soundex_descr :: String
  , flow_cents :: Int -- Positive for money coming in, negative for going out
  , flow :: String -- Formatted as decimal string
  , running_balance_cents :: Int -- Running balance in cents
  , running_balance :: String -- Formatted as decimal string
  , created_at_unix :: Int
  , created_at_utc :: String
  , created_at_tz :: String
  , updated_at_unix :: Int
  , updated_at_utc :: String
  , updated_at_tz :: String
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
  , soundexDescr :: String
  , flowCents :: Int -- Positive for money coming in, negative for going out
  , flow :: String -- Formatted as decimal string
  , runningBalanceCents :: Int -- Running balance in cents
  , runningBalance :: String -- Formatted as decimal string
  , createdAtUnix :: Int
  , createdAtUtc :: String
  , createdAtTz :: String
  , updatedAtUnix :: Int
  , updatedAtUtc :: String
  , updatedAtTz :: String
  }

derive instance Eq LedgerViewRowDB
instance Show LedgerViewRowDB where
  show (LedgerViewRowDB row) =
    "LedgerViewRowDB { transactionId: " <> show row.transactionId
      <> ", descr: \""
      <> row.descr
      <> "\""
      <> ", flowCents: "
      <> show row.flowCents
      <> ", runningBalanceCents: "
      <> show row.runningBalanceCents
      <> " }"

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
  , soundexDescr: row.soundex_descr
  , flowCents: row.flow_cents
  , flow: row.flow
  , runningBalanceCents: row.running_balance_cents
  , runningBalance: row.running_balance
  , createdAtUnix: row.created_at_unix
  , createdAtUtc: row.created_at_utc
  , createdAtTz: row.created_at_tz
  , updatedAtUnix: row.updated_at_unix
  , updatedAtUtc: row.updated_at_utc
  , updatedAtTz: row.updated_at_tz
  }

-- | Get ledger view rows for a specific account ID with optional filters
getLedgerViewRows :: Int -> LedgerViewFilters -> SQLite3.DBConnection -> Aff (Array LedgerViewRowDB)
getLedgerViewRows accountId filters db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/LedgerView/sql/getLedgerViewRows.sql"
  let
    params :: LedgerViewQueryParams
    params =
      { "$accountId": accountId
      , "$descriptionFilter": toNullable filters.description
      , "$minAmountCents": toNullable filters.minAmount
      , "$maxAmountCents": toNullable filters.maxAmount
      , "$unknownExpensesOnly": fromMaybe false filters.unknownExpensesOnly
      }
  rows <- queryObjectDB db sql params
  fromDbRows "ledger_view" rowToLedgerViewRowDB rows
