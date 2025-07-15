module Server.DB.Categories.Queries
  ( CategoryDB(..)
  , getCategoryById
  , getAllCategories
  ) where

import Prelude

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names
newtype CategoryRow = CategoryRow
  { category_id :: Int
  , name :: String
  , created_at :: Int
  , updated_at :: Int
  }

derive instance Generic CategoryRow _
derive newtype instance ReadForeign CategoryRow

-- | Domain type with proper PureScript naming
newtype CategoryDB = CategoryDB
  { categoryId :: Int
  , name :: String
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Generic CategoryDB _
derive newtype instance Show CategoryDB
derive newtype instance Eq CategoryDB
derive newtype instance WriteForeign CategoryDB
derive newtype instance ReadForeign CategoryDB

-- | Convert database row to domain type
rowToCategory :: CategoryRow -> CategoryDB
rowToCategory (CategoryRow row) = CategoryDB
  { categoryId: row.category_id
  , name: row.name
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

-- | Get a category by ID
getCategoryById :: Int -> SQLite3.DBConnection -> Aff (Maybe CategoryDB)
getCategoryById categoryId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Categories/sql/getCategoryById.sql"
  rows <- SQLite3.queryDB db sql [unsafeToForeign categoryId]
  categories <- fromDbRows "categories" rowToCategory rows
  pure $ head categories

-- | Get all categories
getAllCategories :: SQLite3.DBConnection -> Aff (Array CategoryDB)
getAllCategories db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Categories/sql/getAllCategories.sql"
  rows <- SQLite3.queryDB db sql []
  fromDbRows "categories" rowToCategory rows