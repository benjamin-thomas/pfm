{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Category
  ( CategoryRow (..)
  , getCategories
  , getNonStaleCategories
  ) where

import Database.SQLite.Simple
  ( Connection
  , FromRow (fromRow)
  , field
  , query_
  )
import Text.RawString.QQ (r)

data CategoryRow = MkCategoryRow
  { categoryRowId :: Int
  , categoryRowName :: String
  , categoryRowCreatedAt :: Int
  , categoryRowUpdatedAt :: Int
  }

instance FromRow CategoryRow where
  fromRow =
    MkCategoryRow
      <$> field
      <*> field
      <*> field
      <*> field

-- Get all categories
getCategories :: Connection -> IO [CategoryRow]
getCategories conn = query_ conn sql :: IO [CategoryRow]
 where
  sql =
    [r|
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
|]

-- Get only categories that have been updated in the last 90 days
getNonStaleCategories :: Connection -> IO [CategoryRow]
getNonStaleCategories conn = query_ conn sql :: IO [CategoryRow]
 where
  sql =
    [r|
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
 WHERE updated_at > (strftime('%s', 'now') - 90 * 24 * 60 * 60)
|]
