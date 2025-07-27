module Server.DB.LedgerView.Queries
  ( getLedgerViewRows
  , getLedgerViewRowsAsDTO
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import SQLite3 as SQLite3
import Server.Conversion (dbLedgerViewRowToLedgerViewRow)
import Server.DB.LedgerView (LedgerViewRowDB, LedgerViewFilters)
import Server.DB.LedgerView as LedgerView
import Shared.Types (LedgerViewRow)

-- | Empty filters (to get all rows)
emptyFilters :: LedgerViewFilters
emptyFilters =
  { description: Nothing
  , soundexDescr: Nothing
  , minAmount: Nothing
  , maxAmount: Nothing
  , unknownExpensesOnly: Nothing
  }

-- | Get ledger view rows for a specific account ID (returns DB types)
getLedgerViewRows :: Int -> SQLite3.DBConnection -> Aff (Array LedgerViewRowDB)
getLedgerViewRows accountId db = do
  LedgerView.getLedgerViewRows accountId emptyFilters db

-- | Get ledger view rows for a specific account ID with optional filters (with DTO conversion)
getLedgerViewRowsAsDTO :: Int -> LedgerViewFilters -> SQLite3.DBConnection -> Aff (Array LedgerViewRow)
getLedgerViewRowsAsDTO accountId filters db = do
  dbRows <- LedgerView.getLedgerViewRows accountId filters db
  pure $ map dbLedgerViewRowToLedgerViewRow dbRows