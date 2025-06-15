{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.LedgerView
  ( LedgerViewRow (..)
  , getLedgerViewRows
  , AccountId (MkAccountId)
  ) where

import Database.SQLite.Simple (Connection, FromRow (..), field, query)
import Text.RawString.QQ (r)

newtype AccountId = MkAccountId Int

data LedgerViewRow = MkLedgerViewRow
  { lvrTransactionId :: Int
  , lvrFromAccountId :: Int
  , lvrFromAccountName :: String
  , lvrToAccountId :: Int
  , lvrToAccountName :: String
  , lvrDateUnix :: Int
  , lvrDate :: String
  , lvrDescr :: String
  , lvrFlowCents :: Int
  , lvrFlow :: String
  , lvrRunningBalanceCents :: Int
  , lvrRunningBalance :: String
  , lvrCreatedAtUnix :: Int
  , lvrCreatedAtUtc :: String
  , lvrCreatedAtTz :: String
  , lvrUpdatedAtUnix :: Int
  , lvrUpdatedAtUtc :: String
  , lvrUpdatedAtTz :: String
  }

instance FromRow LedgerViewRow where
  fromRow =
    MkLedgerViewRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- | Get transactions with running balance for a specific account
getLedgerViewRows :: Connection -> AccountId -> IO [LedgerViewRow]
getLedgerViewRows conn (MkAccountId accountId) = do
  query
    conn
    sql
    (accountId, accountId, accountId) ::
    IO [LedgerViewRow]
 where
  sql =
    [r|
SELECT y.transaction_id
     , y.from_account_id
     , y.from_account_name
     , y.to_account_id
     , y.to_account_name
     , y.date AS date_unix
     , date(y.date, 'unixepoch') AS date
     , y.descr
     , y.flow_cents
     , printf("%.2f", y.flow_cents / 100.0) AS flow
     , y.running_balance_cents
     , printf("%.2f", y.running_balance_cents / 100.0) AS running_balance
     , y.created_at AS created_at_unix
     , datetime(y.created_at, 'unixepoch') AS created_at_utc
     , datetime(y.created_at, 'unixepoch', 'localtime') AS created_at_tz
     , y.updated_at AS updated_at_unix
     , datetime(y.updated_at, 'unixepoch') AS updated_at_utc
     , datetime(y.updated_at, 'unixepoch', 'localtime') AS updated_at_tz
FROM (
        SELECT x.*
      , SUM(x.flow_cents) OVER (ORDER BY x.transaction_id) AS running_balance_cents
        FROM (
        SELECT t.transaction_id
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

        WHERE t.to_account_id = ? OR t.from_account_id = ?
        )x
)y

ORDER BY y.transaction_id
;
|]
