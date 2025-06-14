{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module DTO.Ledger
    ( LedgerLineSummaryDTO (..)
    , toLedgerLineSummaryDTO
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Domain.Ledger (LedgerLineSummary (..))
import Elm
import GHC.Generics (Generic)

-- | DTO wrapper for WithRunningBalance
data LedgerLineSummaryDTO = MkLedgerLineSummaryDTO
    { ledgerLineSummaryDtoTransactionId :: Int
    , ledgerLineSummaryDtoFromAccountId :: Int
    , ledgerLineSummaryDtoFromAccountName :: String
    , ledgerLineSummaryDtoToAccountId :: Int
    , ledgerLineSummaryDtoToAccountName :: String
    , ledgerLineSummaryDtoDateUnix :: Int
    , ledgerLineSummaryDtoDate :: String
    , ledgerLineSummaryDtoDescr :: String
    , ledgerLineSummaryDtoFlowCents :: Int
    , ledgerLineSummaryDtoFlow :: String
    , ledgerLineSummaryDtoRunningBalanceCents :: Int
    , ledgerLineSummaryDtoRunningBalance :: String
    , ledgerLineSummaryDtoCreatedAtUnix :: Int
    , ledgerLineSummaryDtoCreatedAtUtc :: String
    , ledgerLineSummaryDtoCreatedAtTz :: String
    , ledgerLineSummaryDtoUpdatedAtUnix :: Int
    , ledgerLineSummaryDtoUpdatedAtUtc :: String
    , ledgerLineSummaryDtoUpdatedAtTz :: String
    }
    deriving stock (Generic)

instance Elm LedgerLineSummaryDTO
instance ToJSON LedgerLineSummaryDTO
instance FromJSON LedgerLineSummaryDTO

toLedgerLineSummaryDTO :: LedgerLineSummary -> LedgerLineSummaryDTO
toLedgerLineSummaryDTO ledgerLineSummary =
    MkLedgerLineSummaryDTO
        { ledgerLineSummaryDtoTransactionId = ledgerLineSummaryTransactionId ledgerLineSummary
        , ledgerLineSummaryDtoFromAccountId = ledgerLineSummaryFromAccountId ledgerLineSummary
        , ledgerLineSummaryDtoFromAccountName = ledgerLineSummaryFromAccountName ledgerLineSummary
        , ledgerLineSummaryDtoToAccountId = ledgerLineSummaryToAccountId ledgerLineSummary
        , ledgerLineSummaryDtoToAccountName = ledgerLineSummaryToAccountName ledgerLineSummary
        , ledgerLineSummaryDtoDateUnix = ledgerLineSummaryDateUnix ledgerLineSummary
        , ledgerLineSummaryDtoDate = ledgerLineSummaryDate ledgerLineSummary
        , ledgerLineSummaryDtoDescr = ledgerLineSummaryDescr ledgerLineSummary
        , ledgerLineSummaryDtoFlowCents = ledgerLineSummaryFlowCents ledgerLineSummary
        , ledgerLineSummaryDtoFlow = ledgerLineSummaryFlow ledgerLineSummary
        , ledgerLineSummaryDtoRunningBalanceCents = ledgerLineSummaryRunningBalanceCents ledgerLineSummary
        , ledgerLineSummaryDtoRunningBalance = ledgerLineSummaryRunningBalance ledgerLineSummary
        , ledgerLineSummaryDtoCreatedAtUnix = ledgerLineSummaryCreatedAtUnix ledgerLineSummary
        , ledgerLineSummaryDtoCreatedAtUtc = ledgerLineSummaryCreatedAtUtc ledgerLineSummary
        , ledgerLineSummaryDtoCreatedAtTz = ledgerLineSummaryCreatedAtTz ledgerLineSummary
        , ledgerLineSummaryDtoUpdatedAtUnix = ledgerLineSummaryUpdatedAtUnix ledgerLineSummary
        , ledgerLineSummaryDtoUpdatedAtUtc = ledgerLineSummaryUpdatedAtUtc ledgerLineSummary
        , ledgerLineSummaryDtoUpdatedAtTz = ledgerLineSummaryUpdatedAtTz ledgerLineSummary
        }