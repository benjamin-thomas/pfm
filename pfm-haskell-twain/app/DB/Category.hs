{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.Category
  ( CategoryRow (..)
  , getCategories
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
