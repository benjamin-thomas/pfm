{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Transactions.Queries where

import Data.FileEmbed (embedFile)
import Data.Text.Encoding
import Database.SQLite.Simple
import GHC.Generics

data TransactionNewRow = MkTransactionNewRow
  { fromAccountId :: Int
  , toAccountId :: Int
  , date :: Int
  , descr :: String
  , cents :: Int
  }
  deriving (Generic, ToRow, FromRow)

insertTransaction :: Connection -> TransactionNewRow -> IO ()
insertTransaction conn =
  execute conn $
    Query $
      decodeUtf8 $(embedFile "app/DB/Transactions/insert.sql")