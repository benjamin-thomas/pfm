{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.Ledger
    ( LedgerLine (..)
    , fromLedgerViewRow
    , Suggestion (..)
    , SuggestionForTransaction (..)
    , toSuggestionForTransactionDTO
    ) where

import DB.LedgerView.Queries (LedgerViewRow (..))
import DB.Transactions.Queries
    ( SuggestForTransactionRow
        ( MkSuggestForTransactionRow
        , sftSuggestions
        , sftTransactionId
        )
    , SuggestionJsonDB (..)
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

data Suggestion = MkSuggestion
    { suggestionAccountId :: Int
    , suggestionAccountName :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm)

instance ToJSON Suggestion where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "suggestion")
                }

data SuggestionForTransaction = MkSuggestionForTransaction
    { sftTransactionId :: Int
    , sftSuggestions :: [Suggestion]
    }
    deriving stock (Show, Generic)
    deriving anyclass (Elm)

instance ToJSON SuggestionForTransaction where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "sft")
                }

toSuggestionDto :: SuggestionJsonDB -> Suggestion
toSuggestionDto MkSuggestionJsonDB{..} =
    MkSuggestion
        { suggestionAccountId = suggestionAccountId
        , suggestionAccountName = suggestionAccountName
        }

-- FIXME: move this to another module...
toSuggestionForTransactionDTO :: SuggestForTransactionRow -> SuggestionForTransaction
toSuggestionForTransactionDTO MkSuggestForTransactionRow{..} =
    MkSuggestionForTransaction
        { sftTransactionId = sftTransactionId
        , sftSuggestions = map toSuggestionDto sftSuggestions
        }
