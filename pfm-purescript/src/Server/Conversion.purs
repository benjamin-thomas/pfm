module Server.Conversion
  ( dbLedgerViewRowToLedgerViewRow
  ) where

import Prelude

import Data.Int (toNumber)
import Server.DB.LedgerView (LedgerViewRowDB(..))
import Shared.Types (LedgerViewRow(..))

-- | Convert database ledger view row to DTO
dbLedgerViewRowToLedgerViewRow :: LedgerViewRowDB -> LedgerViewRow
dbLedgerViewRowToLedgerViewRow (LedgerViewRowDB dbRow) =
  LedgerViewRow
    { transactionId: dbRow.transactionId
    , budgetId: dbRow.budgetId
    , fromAccountId: dbRow.fromAccountId
    , fromAccountName: dbRow.fromAccountName
    , toAccountId: dbRow.toAccountId
    , toAccountName: dbRow.toAccountName
    , date: dbRow.date -- Already formatted by SQLite
    , description: dbRow.descr
    , flowAmount: centsToNumber dbRow.flowCents
    , runningBalance: centsToNumber dbRow.runningBalanceCents
    }
  where
  centsToNumber :: Int -> Number
  centsToNumber cents = (toNumber cents) / 100.0