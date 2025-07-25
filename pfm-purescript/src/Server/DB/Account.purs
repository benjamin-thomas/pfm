module Server.DB.Account
  ( AccountDB(..)
  , AccountBalanceDB(..)
  , getAllAccounts
  , getAccountBalances
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String.Common (joinWith)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import SQLite3 as SQLite3
import Server.DB.Utils (fromDbRows)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

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

-- | Account balance row from database query
newtype AccountBalanceRow = MkAccountBalanceRow
  { account_id :: Int
  , category_id :: Int
  , category_name :: String
  , account_name :: String
  , account_balance :: Int
  }

derive instance Generic AccountBalanceRow _
derive newtype instance ReadForeign AccountBalanceRow

-- | Account balance data from database
newtype AccountBalanceDB = AccountBalanceDB
  { accountId :: Int
  , categoryId :: Int
  , categoryName :: String
  , accountName :: String
  , accountBalance :: Int
  }

derive instance Generic AccountBalanceDB _
derive newtype instance ReadForeign AccountBalanceDB
derive newtype instance WriteForeign AccountBalanceDB

-- | Convert row to AccountBalanceDB
rowToAccountBalance :: AccountBalanceRow -> AccountBalanceDB
rowToAccountBalance (MkAccountBalanceRow row) = AccountBalanceDB
  { accountId: row.account_id
  , categoryId: row.category_id
  , categoryName: row.category_name
  , accountName: row.account_name
  , accountBalance: row.account_balance
  }

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

-- | Get account balances for specified account IDs
getAccountBalances :: Array Int -> SQLite3.DBConnection -> Aff (Array AccountBalanceDB)
getAccountBalances accountIds db = do
  let
    questions = joinWith ", " $ map (const "?") accountIds
    -- Parameters for the query (the account IDs)
    params = map writeImpl accountIds

  -- Read SQL from file and replace placeholder
  sqlTemplate <- FS.readTextFile UTF8 "src/Server/DB/Accounts/sql/getAccountBalances.sql"
  let sql = replace (Pattern "__QUESTIONS__") (Replacement questions) sqlTemplate

  -- Execute the query and convert the results
  rows <- SQLite3.queryDB db sql params
  fromDbRows "account_balances" rowToAccountBalance rows