module Server.DB.Account
  ( AccountDB(..)
  , getAllAccounts
  ) where

import Prelude

import Data.Array (head)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

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

-- | Get all accounts
getAllAccounts :: SQLite3.DBConnection -> Aff (Array AccountDB)
getAllAccounts db = do
  rows <- SQLite3.queryDB db "SELECT account_id, category_id, name, created_at, updated_at FROM accounts ORDER BY category_id, name" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToAccount rowArray
  where
  rowToAccount :: Foreign -> AccountDB
  rowToAccount row =
    let
      obj = unsafeFromForeign row :: { account_id :: Int, category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
    in
      AccountDB { accountId: obj.account_id, categoryId: obj.category_id, name: obj.name, createdAtUnix: obj.created_at, updatedAtUnix: obj.updated_at }