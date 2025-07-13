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