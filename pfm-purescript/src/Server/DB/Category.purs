module Server.DB.Category
  ( CategoryDB(..)
  , getAllCategories
  , getCategoryById
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

-- | Get all categories
getAllCategories :: SQLite3.DBConnection -> Aff (Array CategoryDB)
getAllCategories db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories ORDER BY category_id" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToCategory rowArray
  where
  rowToCategory :: Foreign -> CategoryDB
  rowToCategory row =
    let
      obj = unsafeFromForeign row :: { category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
    in
      CategoryDB { categoryId: obj.category_id, name: obj.name, createdAtUnix: obj.created_at, updatedAtUnix: obj.updated_at }

-- | Get category by ID
getCategoryById :: Int -> SQLite3.DBConnection -> Aff (Maybe CategoryDB)
getCategoryById categoryId db = do
  rows <- SQLite3.queryDB db "SELECT category_id, name, created_at, updated_at FROM categories WHERE category_id = ?" [ unsafeToForeign categoryId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { category_id :: Int, name :: String, created_at :: Int, updated_at :: Int }
      pure $ Just $ CategoryDB { categoryId: obj.category_id, name: obj.name, createdAtUnix: obj.created_at, updatedAtUnix: obj.updated_at }