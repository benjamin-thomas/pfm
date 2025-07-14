module Shared.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Yoga.JSON (class ReadForeign, class WriteForeign)

newtype User = User
  { id :: Maybe Int
  , firstName :: String
  , lastName :: String
  }

derive instance Generic User _
derive newtype instance Show User
derive newtype instance Eq User
derive newtype instance ReadForeign User
derive newtype instance WriteForeign User

newtype Transaction = Transaction
  { id :: Int
  , budgetId :: Int
  , fromAccountId :: Int
  , fromAccountName :: String
  , toAccountId :: Int
  , toAccountName :: String
  , uniqueFitId :: Maybe String
  , date :: String -- ISO string for JSON compatibility
  , description :: String
  , amount :: Number -- Keep as Number for JSON compatibility
  }

derive instance Generic Transaction _
derive newtype instance Show Transaction
derive newtype instance Eq Transaction
derive newtype instance ReadForeign Transaction
derive newtype instance WriteForeign Transaction