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

newtype Account = Account
  { accountId :: Int
  , categoryId :: Int
  , name :: String
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Generic Account _
derive newtype instance Show Account
derive newtype instance Eq Account
derive newtype instance ReadForeign Account
derive newtype instance WriteForeign Account

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

newtype LedgerViewRow = LedgerViewRow
  { transactionId :: Int
  , budgetId :: Int
  , fromAccountId :: Int
  , fromAccountName :: String
  , toAccountId :: Int
  , toAccountName :: String
  , dateUnix :: Int
  , date :: String -- ISO date string
  , descr :: String
  , soundexDescr :: String
  , flowCents :: Int
  , flow :: String -- Formatted as decimal string
  , runningBalanceCents :: Int
  , runningBalance :: String -- Formatted as decimal string
  , createdAtUnix :: Int
  , createdAtUtc :: String
  , createdAtTz :: String
  , updatedAtUnix :: Int
  , updatedAtUtc :: String
  , updatedAtTz :: String
  }

derive instance Generic LedgerViewRow _
derive newtype instance Show LedgerViewRow
derive newtype instance Eq LedgerViewRow
derive newtype instance ReadForeign LedgerViewRow
derive newtype instance WriteForeign LedgerViewRow

newtype AccountBalanceRead = AccountBalanceRead
  { accountId :: Int
  , accountName :: String
  , categoryId :: Int
  , categoryName :: String
  , accountBalance :: Int
  }

derive instance Generic AccountBalanceRead _
derive newtype instance Show AccountBalanceRead
derive newtype instance Eq AccountBalanceRead
derive newtype instance ReadForeign AccountBalanceRead
derive newtype instance WriteForeign AccountBalanceRead

newtype SuggestedAccount = SuggestedAccount
  { accountId :: Int
  , accountName :: String
  }

derive instance Generic SuggestedAccount _
derive newtype instance Show SuggestedAccount
derive newtype instance Eq SuggestedAccount
derive newtype instance ReadForeign SuggestedAccount
derive newtype instance WriteForeign SuggestedAccount

newtype Suggestion = Suggestion
  { soundexDescr :: String
  , suggestedAccounts :: Array SuggestedAccount
  }

derive instance Generic Suggestion _
derive newtype instance Show Suggestion
derive newtype instance Eq Suggestion
derive newtype instance ReadForeign Suggestion
derive newtype instance WriteForeign Suggestion