module Server.Conversion
  ( dbLedgerViewRowToLedgerViewRow
  ) where

import Prelude

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
    , dateUnix: dbRow.dateUnix
    , date: dbRow.date -- Already formatted by SQLite
    , descr: dbRow.descr
    , soundexDescr: dbRow.soundexDescr
    , flowCents: dbRow.flowCents
    , flow: dbRow.flow
    , runningBalanceCents: dbRow.runningBalanceCents
    , runningBalance: dbRow.runningBalance
    , createdAtUnix: dbRow.createdAtUnix
    , createdAtUtc: dbRow.createdAtUtc
    , createdAtTz: dbRow.createdAtTz
    , updatedAtUnix: dbRow.updatedAtUnix
    , updatedAtUtc: dbRow.updatedAtUtc
    , updatedAtTz: dbRow.updatedAtTz
    }