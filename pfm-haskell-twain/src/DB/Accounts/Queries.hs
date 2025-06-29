{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Accounts.Queries where

import Data.FileEmbed (embedFile)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple
import GHC.Generics

data AccountReadRow = MkAccountReadRow
    { accountId :: Int
    , categoryId :: Int
    , categoryName :: Text
    , accountName :: Text
    }
    deriving (Generic, FromRow, Show)

{-

Temporary, for GHCi exploration.

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m +DB.Accounts.Queries
ghci> newConn >>= getAll >>= mapM_ print

 -}
getAll :: Connection -> IO [AccountReadRow]
getAll conn = query_ conn $ Query $ decodeUtf8 $(embedFile "src/DB/Accounts/getAll.sql")

{-

ghci> :m +DB.Accounts.Queries
ghci> newConn >>= getBalance 2

 -}
getBalance :: Int -> Connection -> IO Int
getBalance accountId' conn = do
    result <- query conn sql (Only accountId')
    case result of
        [Only balance] -> pure balance
        _ -> pure 0
  where
    sql = Query (decodeUtf8 $(embedFile "src/DB/Accounts/getBalance.sql"))

-- instance FromRow (AccountReadRow, Int) where
--     fromRow =
--         let accountFromRow =
--                 MkAccountReadRow
--                     <$> field
--                     <*> field
--                     <*> field
--                     <*> field

--             balanceFromRow = field
--          in (,) <$> accountFromRow <*> balanceFromRow

getBalances :: [Int] -> Connection -> IO [(AccountReadRow, Int)]
getBalances accountIds conn = do
    let rowParser = do
            account <- fromRow
            balance <- field
            pure (account, balance)

    queryWith rowParser conn sql accountIds
  where
    sql =
        let questions = intersperse ',' (map (const '?') accountIds)
         in Query
                . T.replace "__QUESTIONS__" (T.pack questions)
                $ decodeUtf8 $(embedFile "src/DB/Accounts/getBalances.sql")