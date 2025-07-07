{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module DTO.TransactionWrite
  ( TransactionWrite
  , toTransactionNewRow
  , SuggestionWrite
  , toSuggestionsInsert
  ) where

import DB.Transactions.Queries (SuggestionInsert (MkSuggestionInsert, siToAccountId, siTransactionId), TransactionNewRow (..))
import DTO.Utils
import Data.Aeson (FromJSON, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Aeson.Types (FromJSON (parseJSON))
import Elm.Generic (Elm)
import GHC.Generics (Generic)

data TransactionWrite = MkTransactionWrite
  { twBudgetId :: Maybe Int -- present if editing
  , twFromAccountId :: Int
  , twToAccountId :: Int
  , twDateUnix :: Int
  , twDescr :: String
  , twCents :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Elm)

instance FromJSON TransactionWrite where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropAndLowerHead (length "tw")
        }

toTransactionNewRow :: TransactionWrite -> TransactionNewRow
toTransactionNewRow tx =
  MkTransactionNewRow
    { budgetId = twBudgetId tx
    , fromAccountId = twFromAccountId tx
    , toAccountId = twToAccountId tx
    , uniqueFitId = Nothing
    , date = twDateUnix tx
    , descr = twDescr tx
    , cents = twCents tx
    }

data SuggestionWrite = MkSuggestionWrite
  { swTransactionId :: Int
  , swToAccountId :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (Elm)

instance FromJSON SuggestionWrite where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = dropAndLowerHead (length "sw")
        }

toSuggestionsInsert :: SuggestionWrite -> SuggestionInsert
toSuggestionsInsert sw =
  MkSuggestionInsert
    { siTransactionId = swTransactionId sw
    , siToAccountId = swToAccountId sw
    }