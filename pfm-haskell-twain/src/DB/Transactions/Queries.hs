{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Transactions.Queries where

import Crypto.Sha256 qualified as Sha256
import Data.ByteString.Base16 qualified as Base16
import Data.FileEmbed (embedFile)
import Data.Text
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField

data TransactionNewRow = MkTransactionNewRow
  { fromAccountId :: Int
  , toAccountId :: Int
  , uniqueFitId :: Maybe UniqueFitId
  , date :: Int
  , descr :: String
  , cents :: Int
  }

newtype UniqueFitId = MkUniqueFitId Text deriving (Show)

instance ToField UniqueFitId where
  toField (MkUniqueFitId t) = toField t

insertTransaction :: Connection -> TransactionNewRow -> IO ()
insertTransaction conn newRow =
  let uniqueFitId' = uniqueFitId newRow
      hexDigest = case uniqueFitId' of
        Nothing -> T.pack ""
        Just (MkUniqueFitId t) -> TE.decodeUtf8 $ Base16.encode $ Sha256.hash $ TE.encodeUtf8 t

      toAccountId' =
        if hexDigest == T.pack "2dae108a57793798de95a6c46c43250a578661e7a477979dd319eed94c4d11d6"
          then 11 -- TEMP: to test classification suggestions after reimport
          else toAccountId newRow
   in execute
        conn
        (Query $ decodeUtf8 $(embedFile "src/DB/Transactions/insert.sql"))
        ( fromAccountId newRow
        , toAccountId'
        , uniqueFitId'
        , date newRow
        , descr newRow -- descr_orig
        , descr newRow
        , cents newRow
        )

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

data SuggestToAccountIdRow = MkSuggestToAccountIdRow
  { transactionId :: Int
  , jsonValue :: Text
  }
  deriving (Show)

instance FromRow SuggestToAccountIdRow where
  fromRow =
    MkSuggestToAccountIdRow
      <$> field
      <*> field

{-

ghci> :m +DB.Transactions.Queries
ghci> _newConn >>= suggestToAccountId 2 10

 -}
suggestToAccountId :: Int -> Int -> Connection -> IO [SuggestToAccountIdRow]
suggestToAccountId fromAccountId' toAccountId' conn =
  query conn sql (fromAccountId', toAccountId', fromAccountId', toAccountId')
 where
  sql = Query $ decodeUtf8 $(embedFile "src/DB/Transactions/suggest.sql")
