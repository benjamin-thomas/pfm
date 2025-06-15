{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.User
    ( UserRow (..)
    , getUserAllRows
    , getNewPlatformUserRows
    ) where

import Data.Text
import Database.SQLite.Simple
    ( Connection
    , FromRow (fromRow)
    , field
    , query_
    )
import Text.RawString.QQ (r)

data UserRow = MkUserRow
    { userId :: Int
    , firstName :: Text
    , lastName :: Text
    , email :: Text
    , createdAt :: Int
    , updatedAt :: Int
    }

instance FromRow UserRow where
    fromRow =
        MkUserRow
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-- Get all users
getUserAllRows :: Connection -> IO [UserRow]
getUserAllRows conn = query_ conn sql :: IO [UserRow]
  where
    sql =
        [r|
SELECT user_id
     , first_name
     , last_name
     , email
     , created_at
     , updated_at
  FROM users
|]

-- Get only users who joined after June 1, 2025 (new platform users)
getNewPlatformUserRows :: Connection -> IO [UserRow]
getNewPlatformUserRows conn = query_ conn sql :: IO [UserRow]
  where
    sql =
        [r|
SELECT user_id
     , first_name
     , last_name
     , email
     , created_at
     , updated_at
  FROM users
 WHERE created_at > strftime('%s', '2025-06-01')
|]
