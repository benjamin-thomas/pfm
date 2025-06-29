{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
      decodeUtf8 $(embedFile "src/DB/Transactions/insert.sql")

-- instance ToRow (Int, TransactionNewRow) where
--   toRow (transactionId, newRow) =
--     [ toField $ fromAccountId newRow
--     , toField $ toAccountId newRow
--     , toField $ date newRow
--     , toField $ descr newRow
--     , toField $ cents newRow
--     , toField transactionId
--     ]

-- updateTransaction :: Connection -> (Int, TransactionNewRow) -> IO ()
-- updateTransaction conn =
--   execute conn $
--     Query $
--       decodeUtf8 $(embedFile "src/DB/Transactions/update.sql")

updateTransaction :: Connection -> (Int, TransactionNewRow) -> IO ()
updateTransaction conn (transactionId, newRow) =
  execute
    conn
    (Query $ decodeUtf8 $(embedFile "src/DB/Transactions/update.sql"))
    ( fromAccountId newRow
    , toAccountId newRow
    , date newRow
    , descr newRow
    , cents newRow
    , transactionId
    )

deleteTransaction :: Connection -> Int -> IO ()
deleteTransaction conn transactionId =
  execute
    conn
    (Query $ decodeUtf8 $(embedFile "src/DB/Transactions/delete.sql"))
    (Only transactionId)

deleteAllTransactions :: Connection -> IO ()
deleteAllTransactions conn =
  execute
    conn
    (Query $ decodeUtf8 $(embedFile "src/DB/Transactions/deleteAll.sql"))
    ()
