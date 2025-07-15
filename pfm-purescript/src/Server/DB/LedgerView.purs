module Server.DB.LedgerView
  ( LedgerViewRowDB(..)
  , getLedgerViewRows
  ) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import SQLite3 as SQLite3

-- | Database representation of a ledger view row
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

-- | Get ledger view rows for a specific account ID
getLedgerViewRows :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRowDB)
getLedgerViewRows accountId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/LedgerView/sql/getLedgerViewRows.sql"
  rows <- SQLite3.queryDB db sql [ unsafeToForeign accountId, unsafeToForeign accountId, unsafeToForeign accountId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToLedgerViewRow rowArray
  where

  rowToLedgerViewRow :: Foreign -> LedgerViewRowDB
  rowToLedgerViewRow row =
    let
      obj = unsafeFromForeign row :: 
        { transaction_id :: Int
        , budget_id :: Int
        , from_account_id :: Int
        , from_account_name :: String
        , to_account_id :: Int
        , to_account_name :: String
        , date_unix :: Int
        , date :: String
        , descr :: String
        , flow_cents :: Int
        , running_balance_cents :: Int
        , created_at_unix :: Int
        , updated_at_unix :: Int
        }
    in
      LedgerViewRowDB
        { transactionId: obj.transaction_id
        , budgetId: obj.budget_id
        , fromAccountId: obj.from_account_id
        , fromAccountName: obj.from_account_name
        , toAccountId: obj.to_account_id
        , toAccountName: obj.to_account_name
        , dateUnix: obj.date_unix
        , date: obj.date
        , descr: obj.descr
        , flowCents: obj.flow_cents
        , runningBalanceCents: obj.running_balance_cents
        , createdAtUnix: obj.created_at_unix
        , updatedAtUnix: obj.updated_at_unix
        }