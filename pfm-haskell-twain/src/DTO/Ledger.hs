{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.Ledger
    ( LedgerLine (..)
    , fromLedgerViewRow
    , Suggestion (..)
    , SuggestedAccount (..)
    , toSuggestionDTO
    ) where

import DB.LedgerView.Queries (LedgerViewRow (..))
import DB.Transactions.Queries
    ( SuggestedAccountJsonDb (..)
    , SuggestionRow
        ( MkSuggestionRow
        , srSoundexDescr
        , srSuggestedAccounts
        )
    )
import DTO.Utils
import Data.Aeson (FromJSON, Options (fieldLabelModifier), ToJSON (toJSON), defaultOptions, genericToJSON)
import Data.Text (Text)
import Elm
import GHC.Generics (Generic)

data LedgerLine = MkLedgerLine
    { llsTransactionId :: Int
    , llsFromAccountId :: Int
    , llsFromAccountName :: String
    , llsToAccountId :: Int
    , llsToAccountName :: String
    , llsDateUnix :: Int
    , llsDate :: String
    , llsDescr :: String
    , llsSoundexDescr :: String
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

instance ToJSON LedgerLine where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "lls")
                }

fromLedgerViewRow :: LedgerViewRow -> LedgerLine
fromLedgerViewRow MkLedgerViewRow{..} =
    MkLedgerLine
        { llsTransactionId = lvrTransactionId
        , llsFromAccountId = lvrFromAccountId
        , llsFromAccountName = lvrFromAccountName
        , llsToAccountId = lvrToAccountId
        , llsToAccountName = lvrToAccountName
        , llsDateUnix = lvrDateUnix
        , llsDate = lvrDate
        , llsDescr = lvrDescr
        , llsSoundexDescr = lvrSoundexDescr
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

data SuggestedAccount = MkSuggestedAccount
    { saAccountId :: Int
    , saAccountName :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm)

instance ToJSON SuggestedAccount where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "sa")
                }

data Suggestion = MkSuggestion
    { sSoundexDescr :: Text
    , sSuggestedAccounts :: [SuggestedAccount]
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm)

instance ToJSON Suggestion where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "s")
                }

toSuggestedAccount :: SuggestedAccountJsonDb -> SuggestedAccount
toSuggestedAccount MkSuggestedAccountJsonDb{..} =
    MkSuggestedAccount
        { saAccountId = saAccountId
        , saAccountName = saAccountName
        }

-- FIXME: move this to another module...
toSuggestionDTO :: SuggestionRow -> Suggestion
toSuggestionDTO MkSuggestionRow{..} =
    MkSuggestion
        { sSoundexDescr = srSoundexDescr
        , sSuggestedAccounts = map toSuggestedAccount srSuggestedAccounts
        }
