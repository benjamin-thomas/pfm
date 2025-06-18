{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Accounts.Queries where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple
import GHC.Generics

data AccountReadRow = MkAccountReadRow
    { accountId :: Int
    , categoryId :: Int
    , categoryName :: Text
    , name :: Text
    }
    deriving (Generic, FromRow, Show)

{-

Temporary, for GHCi exploration.

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m +DB.Accounts.Queries
ghci> newConn >>= getAll >>= mapM_ print

 -}
getAll :: Connection -> IO [AccountReadRow]
getAll conn = query_ conn $ Query $ decodeUtf8 $(embedFile "app/DB/Accounts/getAll.sql")