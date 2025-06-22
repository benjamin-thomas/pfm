{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module DTO.TransactionWrite
  ( TransactionWrite
  , toTransactionNewRow
  ) where

import DB.Transactions.Queries (TransactionNewRow (..))
import DTO.Utils
import Data.Aeson (FromJSON, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Aeson.Types (FromJSON (parseJSON))
import Elm.Generic (Elm)
import GHC.Generics (Generic)

data TransactionWrite = MkTransactionWrite
  { twFromAccountId :: Int
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
    { fromAccountId = twFromAccountId tx
    , toAccountId = twToAccountId tx
    , date = twDateUnix tx
    , descr = twDescr tx
    , cents = twCents tx
    }
