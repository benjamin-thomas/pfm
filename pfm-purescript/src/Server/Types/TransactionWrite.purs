module Server.Types.TransactionWrite where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- Type for creating/updating transactions via API
newtype TransactionWrite = TransactionWrite
  { budgetId :: Maybe Int       -- present if editing
  , fromAccountId :: Int
  , toAccountId :: Int
  , dateUnix :: Int
  , descr :: String
  , cents :: Int                -- amount in cents
  }

derive instance Generic TransactionWrite _
derive newtype instance Show TransactionWrite
derive newtype instance Eq TransactionWrite
derive newtype instance ReadForeign TransactionWrite
derive newtype instance WriteForeign TransactionWrite