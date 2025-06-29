{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.User.Queries
    ( UserRow (..)
    , getAllUserRows
    , getNewPlatformUserRows
    ) where

import Data.FileEmbed (embedFile)
import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple
    ( Connection
    , FromRow (fromRow)
    , Query (Query)
    , field
    , query_
    )

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

ghci> :m + DB.User.Queries
ghci> newConn >>= getAllUserRows

 -}

-- Get all users
getAllUserRows :: Connection -> IO [UserRow]
getAllUserRows conn = query_ conn sql :: IO [UserRow]
  where
    sql = Query (decodeUtf8 $(embedFile "src/DB/User/getAll.sql"))

-- Get only users who joined after June 1, 2025 (new platform users)
getNewPlatformUserRows :: Connection -> IO [UserRow]
getNewPlatformUserRows conn = query_ conn sql :: IO [UserRow]
  where
    sql = Query (decodeUtf8 $(embedFile "src/DB/User/getNewPlatformUsers.sql"))
