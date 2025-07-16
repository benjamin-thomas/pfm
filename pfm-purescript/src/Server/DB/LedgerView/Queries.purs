module Server.DB.LedgerView.Queries
  ( getLedgerViewRows
  , getLedgerViewRowsAsDTO
  ) where

import Prelude

import Data.Functor (map)
import Data.Int (toNumber)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.Conversion (dbLedgerViewRowToLedgerViewRow)
import Server.DB.LedgerView (LedgerViewRowDB)
import Server.DB.LedgerView as LedgerView
import SQLite3 as SQLite3
import Shared.Types (LedgerViewRow)
import Yoga.JSON as JSON

-- | Get ledger view rows for a specific account ID (returns DB types)
getLedgerViewRows :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRowDB)
getLedgerViewRows accountId db = do
  LedgerView.getLedgerViewRows accountId db

-- | Get ledger view rows for a specific account ID (with DTO conversion)
getLedgerViewRowsAsDTO :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRow)
getLedgerViewRowsAsDTO accountId db = do
  dbRows <- LedgerView.getLedgerViewRows accountId db
  pure $ map dbLedgerViewRowToLedgerViewRow dbRows