module Server.DB.LedgerView
  ( LedgerViewRowDB(..)
  , getLedgerViewRows
  ) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
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
  rows <- SQLite3.queryDB db sql [ unsafeToForeign accountId, unsafeToForeign accountId, unsafeToForeign accountId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToLedgerViewRow rowArray
  where
  sql = 
    """
    SELECT y.transaction_id
         , y.budget_id
         , y.from_account_id
         , y.from_account_name
         , y.to_account_id
         , y.to_account_name
         , y.date AS date_unix
         , date(y.date, 'unixepoch') AS date
         , y.descr
         , y.flow_cents
         , y.running_balance_cents
         , y.created_at AS created_at_unix
         , y.updated_at AS updated_at_unix
    FROM (
            SELECT x.*
          , SUM(x.flow_cents) OVER (ORDER BY x.date ASC, x.transaction_id ASC) AS running_balance_cents
            FROM (
            SELECT t.transaction_id
                    , t.budget_id
                    , a.name AS from_account_name
                    , a.account_id AS from_account_id
                    , b.name AS to_account_name
                    , b.account_id AS to_account_id
                    , t.date
                    , t.descr
                    , t.created_at
                    , t.updated_at
                    , t.cents * CASE WHEN t.from_account_id = ? THEN -1 ELSE 1 END AS flow_cents
            FROM transactions AS t

            INNER JOIN accounts AS a
                    ON t.from_account_id = a.account_id

            INNER JOIN accounts AS b
                    ON t.to_account_id = b.account_id

            INNER JOIN budgets AS bu
                    ON t.budget_id = bu.budget_id

            WHERE t.to_account_id = ? OR t.from_account_id = ?
            )x
    )y
    ORDER BY y.date ASC, y.transaction_id ASC
    """

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