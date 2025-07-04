{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Transactions.Queries where

import Crypto.Sha256 qualified as Sha256
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Text
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.Encoding qualified as TE
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow (RowParser)
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

data SuggestedAccountJsonDb = MkSuggestedAccountJsonDb
  { saAccountId :: Int
  , saAccountName :: Text
  , saOccurrenceCount :: Int
  }
  deriving (Show)

instance FromRow SuggestedAccountJsonDb where
  fromRow =
    MkSuggestedAccountJsonDb
      <$> field
      <*> field
      <*> field

data SuggestionRow = MkSuggestionRow
  { srSoundexDescr :: Text
  , srSuggestedAccounts :: [SuggestedAccountJsonDb]
  }
  deriving (Show)

instance FromJSON SuggestedAccountJsonDb where
  parseJSON = withObject "suggestedAccounts" $ \v ->
    MkSuggestedAccountJsonDb
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "occurrences"

instance FromRow SuggestionRow where
  fromRow = do
    soundexDescr <- field
    jsonText <- field :: RowParser Text
    let result = eitherDecode (BL.fromStrict (encodeUtf8 jsonText)) :: Either String [SuggestedAccountJsonDb]
    case result of
      Left x -> error x
      Right suggestions -> pure $ MkSuggestionRow soundexDescr suggestions

{-

ghci> :m +DB.Transactions.Queries
ghci> _newConn >>= getAllSuggestions 2 10

 -}
getAllSuggestions :: Int -> Int -> Connection -> IO [SuggestionRow]
getAllSuggestions fromAccountId' toAccountId' conn =
  query conn sql (fromAccountId', toAccountId')
 where
  sql = Query $ decodeUtf8 $(embedFile "src/DB/Transactions/suggestions.sql")

data SuggestionInsert = MkSuggestionInsert
  { siTransactionId :: Int
  , siToAccountId :: Int
  }
  deriving (Show)

instance ToRow SuggestionInsert where
  toRow (MkSuggestionInsert transactionId toAccountId') =
    [ toField transactionId
    , toField toAccountId'
    ]

insertSuggestions :: Connection -> [SuggestionInsert] -> IO ()
insertSuggestions conn suggestions =
  -- executeMany
  --   conn
  --   (Query $ decodeUtf8 $(embedFile "src/DB/Transactions/insertSuggestions.sql"))
  --   suggestions

  -- Start a transaction
  withTransaction conn $ do
    -- Create and populate temp table
    execute_ conn "CREATE TEMPORARY TABLE tmp (transaction_id INTEGER, to_account_id INTEGER)"

    -- Batch insert all updates
    executeMany
      conn
      "INSERT INTO tmp (transaction_id, to_account_id) VALUES (?,?)"
      suggestions

    -- Perform the update
    execute_ conn $
      Query $
        T.unlines
          [ "UPDATE transactions AS t"
          , "SET to_account_id = tmp.to_account_id"
          , "FROM tmp"
          , "WHERE t.transaction_id = tmp.transaction_id"
          ]

    -- Clean up (optional - temp tables are dropped at end of session)
    execute_ conn "DROP TABLE tmp"
