{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.Ledger
    ( LedgerLineSummary (..)
    , fromLedgerViewRow
    ) where

import DB.LedgerView (LedgerViewRow (..))
import DTO.Utils
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON (toJSON), defaultOptions, genericToJSON)
import Elm
import GHC.Generics (Generic)

data LedgerLineSummary = MkLedgerLineSummary
    { llsTransactionId :: Int
    , llsFromAccountId :: Int
    , llsFromAccountName :: String
    , llsToAccountId :: Int
    , llsToAccountName :: String
    , llsDateUnix :: Int
    , llsDate :: String
    , llsDescr :: String
    , llsFlowCents :: Int
    , llsFlow :: String
    , llsRunningBalanceCents :: Int
    , llsRunningBalance :: String
    , llsCreatedAtUnix :: Int
    , llsCreatedAtUtc :: String
    , llsCreatedAtTz :: String
    , llsUpdatedAtUnix :: Int
    , llsUpdatedAtUtc :: String
    , llsUpdatedAtTz :: String
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm, FromJSON)

instance ToJSON LedgerLineSummary where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "lls")
                }

fromLedgerViewRow :: LedgerViewRow -> LedgerLineSummary
fromLedgerViewRow MkLedgerViewRow{..} =
    MkLedgerLineSummary
        { llsTransactionId = lvrTransactionId
        , llsFromAccountId = lvrFromAccountId
        , llsFromAccountName = lvrFromAccountName
        , llsToAccountId = lvrToAccountId
        , llsToAccountName = lvrToAccountName
        , llsDateUnix = lvrDateUnix
        , llsDate = lvrDate
        , llsDescr = lvrDescr
        , llsFlowCents = lvrFlowCents
        , llsFlow = lvrFlow
        , llsRunningBalanceCents = lvrRunningBalanceCents
        , llsRunningBalance = lvrRunningBalance
        , llsCreatedAtUnix = lvrCreatedAtUnix
        , llsCreatedAtUtc = lvrCreatedAtUtc
        , llsCreatedAtTz = lvrCreatedAtTz
        , llsUpdatedAtUnix = lvrUpdatedAtUnix
        , llsUpdatedAtUtc = lvrUpdatedAtUtc
        , llsUpdatedAtTz = lvrUpdatedAtTz
        }
