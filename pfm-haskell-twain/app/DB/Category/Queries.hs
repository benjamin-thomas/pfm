{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Category.Queries
  ( CategoryRow (..)
  , getCategories
  , getNonStaleCategories
  ) where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple
  ( Connection
  , FromRow (fromRow)
  , field
  , query_
  , Query(Query)
  )

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
  sql = Query (decodeUtf8 $(embedFile "app/DB/Category/getAll.sql"))

-- Get only categories that have been updated in the last 90 days
getNonStaleCategories :: Connection -> IO [CategoryRow]
getNonStaleCategories conn = query_ conn sql :: IO [CategoryRow]
 where
  sql = Query (decodeUtf8 $(embedFile "app/DB/Category/getNonStale.sql"))
