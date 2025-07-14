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

-- Transaction types based on the Elm model
newtype Transaction = Transaction
  { id :: Maybe Int
  , fromAccountId :: Int
  , fromAccountName :: String
  , toAccountId :: Int
  , toAccountName :: String
  , date :: String
  , description :: String
  , amount :: Number -- Using Number for simplicity, can switch to Int for cents later
  }

derive instance Generic Transaction _
derive newtype instance Show Transaction
derive newtype instance Eq Transaction
derive newtype instance ReadForeign Transaction
derive newtype instance WriteForeign Transaction