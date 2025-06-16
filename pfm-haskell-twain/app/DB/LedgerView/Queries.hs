{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.LedgerView.Queries
  ( LedgerViewRow (..)
  , getLedgerViewRows
  , AccountId (MkAccountId)
  ) where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple (Connection, FromRow (..), field, query, Query(Query))

newtype AccountId = MkAccountId Int deriving (Show)

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
  deriving (Show)

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
getLedgerViewRows :: AccountId -> Connection -> IO [LedgerViewRow]
getLedgerViewRows (MkAccountId accountId) conn = do
  query
    conn
    sql
    (accountId, accountId, accountId) ::
    IO [LedgerViewRow]
 where
  sql = Query (decodeUtf8 $(embedFile "app/DB/LedgerView/getLedgerViewRows.sql"))
