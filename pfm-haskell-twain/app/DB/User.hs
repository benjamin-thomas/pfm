{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.User
    ( UserRow (..)
    , getUserRows
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
    fromRow = do
        MkUserRow
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

getUserRows :: Connection -> IO [UserRow]
getUserRows conn = query_ conn sql :: IO [UserRow]
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
