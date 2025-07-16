module Server.DB.Account
  ( AccountDB(..)
  , getAllAccounts
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect.Aff (Aff)
import SQLite3 as SQLite3
import Server.DB.Utils (fromDbRows)
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names
newtype AccountRow = MkAccountRow
  { account_id :: Int
  , category_id :: Int
  , name :: String
  , created_at :: Int
  , updated_at :: Int
  }

derive instance Generic AccountRow _
derive newtype instance ReadForeign AccountRow

-- | Domain type with proper PureScript naming
newtype AccountDB = AccountDB
  { accountId :: Int
  , categoryId :: Int
  , name :: String
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Generic AccountDB _
derive newtype instance Show AccountDB
derive newtype instance Eq AccountDB
derive newtype instance WriteForeign AccountDB
derive newtype instance ReadForeign AccountDB

-- | Convert database row to domain type
rowToAccount :: AccountRow -> AccountDB
rowToAccount (MkAccountRow row) = AccountDB
  { accountId: row.account_id
  , categoryId: row.category_id
  , name: row.name
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

-- | Get all accounts
getAllAccounts :: SQLite3.DBConnection -> Aff (Array AccountDB)
getAllAccounts db = do
  rows <- SQLite3.queryDB db "SELECT account_id, category_id, name, created_at, updated_at FROM accounts ORDER BY category_id, name" []
  fromDbRows "accounts" rowToAccount rows