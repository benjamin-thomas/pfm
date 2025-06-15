{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB.User
    ( UserRow (..)
    , getAllUserRows
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
    deriving (Show) -- show for ghci exploration

instance FromRow UserRow where
    fromRow =
        MkUserRow
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

{-

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m + DB.User
ghci> newConn >>= getAllUserRows

 -}

-- Get all users
getAllUserRows :: Connection -> IO [UserRow]
getAllUserRows conn = query_ conn sql :: IO [UserRow]
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
