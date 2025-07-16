module Server.DB.Category
  ( CategoryDB(..)
  , getAllCategories
  , getCategoryById
  ) where

import Prelude

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import SQLite3 as SQLite3
import Server.DB.Utils (fromDbRows)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as JSON

-- | Database row type matching SQL column names
newtype CategoryRow = MkCategoryRow
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
rowToCategory (MkCategoryRow row) = CategoryDB
  { categoryId: row.category_id
  , name: row.name
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

-- | Get all categories
getAllCategories :: SQLite3.DBConnection -> Aff (Array CategoryDB)
getAllCategories db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories ORDER BY category_id" []
  fromDbRows "categories" rowToCategory rows

-- | Get category by ID
getCategoryById :: Int -> SQLite3.DBConnection -> Aff (Maybe CategoryDB)
getCategoryById categoryId db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories WHERE category_id = ?" [ JSON.writeImpl categoryId ]
  categories <- fromDbRows "categories" rowToCategory rows
  pure $ head categories