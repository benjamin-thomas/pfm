{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Ledger
    ( LedgerLineSummary (..)
    , fromLedgerViewRow
    ) where

import DB.LedgerView (LedgerViewRow (..))
import Data.Aeson (FromJSON, ToJSON)
import Elm
import GHC.Generics (Generic)

{-
Represents the ledger entries from an account's point of view.
-}
data LedgerLineSummary = MkLedgerLineSummary
    { ledgerLineSummaryTransactionId :: Int
    , ledgerLineSummaryFromAccountId :: Int
    , ledgerLineSummaryFromAccountName :: String
    , ledgerLineSummaryToAccountId :: Int
    , ledgerLineSummaryToAccountName :: String
    , ledgerLineSummaryDateUnix :: Int
    , ledgerLineSummaryDate :: String
    , ledgerLineSummaryDescr :: String
    , ledgerLineSummaryFlowCents :: Int
    , ledgerLineSummaryFlow :: String
    , ledgerLineSummaryRunningBalanceCents :: Int
    , ledgerLineSummaryRunningBalance :: String
    , ledgerLineSummaryCreatedAtUnix :: Int
    , ledgerLineSummaryCreatedAtUtc :: String
    , ledgerLineSummaryCreatedAtTz :: String
    , ledgerLineSummaryUpdatedAtUnix :: Int
    , ledgerLineSummaryUpdatedAtUtc :: String
    , ledgerLineSummaryUpdatedAtTz :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm, ToJSON, FromJSON)

fromLedgerViewRow :: LedgerViewRow -> LedgerLineSummary
fromLedgerViewRow
    ( MkLedgerViewRow
            { ledgerViewTransactionId = transactionId
            , ledgerViewFromAccountId = fromAccountId
            , ledgerViewFromAccountName = fromAccountName
            , ledgerViewToAccountId = toAccountId
            , ledgerViewToAccountName = toAccountName
            , ledgerViewDateUnix = dateUnix
            , ledgerViewDate = date
            , ledgerViewDescr = descr
            , ledgerViewFlowCents = flowCents
            , ledgerViewFlow = flow
            , ledgerViewRunningBalanceCents = runningBalanceCents
            , ledgerViewRunningBalance = runningBalance
            , ledgerViewCreatedAtUnix = createdAtUnix
            , ledgerViewCreatedAtUtc = createdAtUtc
            , ledgerViewCreatedAtTz = createdAtTz
            , ledgerViewUpdatedAtUnix = updatedAtUnix
            , ledgerViewUpdatedAtUtc = updatedAtUtc
            , ledgerViewUpdatedAtTz = updatedAtTz
            }
        ) =
        MkLedgerLineSummary
            { ledgerLineSummaryTransactionId = transactionId
            , ledgerLineSummaryFromAccountId = fromAccountId
            , ledgerLineSummaryFromAccountName = fromAccountName
            , ledgerLineSummaryToAccountId = toAccountId
            , ledgerLineSummaryToAccountName = toAccountName
            , ledgerLineSummaryDateUnix = dateUnix
            , ledgerLineSummaryDate = date
            , ledgerLineSummaryDescr = descr
            , ledgerLineSummaryFlowCents = flowCents
            , ledgerLineSummaryFlow = flow
            , ledgerLineSummaryRunningBalanceCents = runningBalanceCents
            , ledgerLineSummaryRunningBalance = runningBalance
            , ledgerLineSummaryCreatedAtUnix = createdAtUnix
            , ledgerLineSummaryCreatedAtUtc = createdAtUtc
            , ledgerLineSummaryCreatedAtTz = createdAtTz
            , ledgerLineSummaryUpdatedAtUnix = updatedAtUnix
            , ledgerLineSummaryUpdatedAtUtc = updatedAtUtc
            , ledgerLineSummaryUpdatedAtTz = updatedAtTz
            }
