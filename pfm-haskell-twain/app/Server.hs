{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server (runServer, exportElm) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, FromRow, field, fromRow, open, query, query_)
import Elm
import GHC.Generics
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Text.RawString.QQ (r)
import Text.Read (readMaybe)
import Web.Twain qualified as Twain

exportElm :: IO ()
exportElm = generateElm @Types $ defaultSettings "../pfm-elm/src/" ["Generated"]

type Types =
    '[ User
     , Category
     , WithRunningBalanceEntity
     ]

data User = User
    { userName :: Text
    , userAge :: Int
    }
    deriving (Generic)
    deriving
        (Elm, ToJSON, FromJSON)
        via ElmStreet User

mkApp :: Connection -> Twain.Application
mkApp conn =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        (logStdoutDev : corsMiddleware : routes conn)

corsMiddleware :: Twain.Middleware
corsMiddleware app req respond = do
    liftIO $ putStrLn $ "Request: " <> show req
    app
        req
        $ respond
            . Twain.withHeader
                ("Access-Control-Allow-Origin", "http://localhost:3000")

echoName :: Twain.ResponderM ()
echoName = do
    name <- Twain.param "name"
    Twain.send $ Twain.html $ "Hello, " <> name

greeting :: ByteString -> Twain.Response
greeting name = Twain.html $ "Hello, " <> name

routes :: Connection -> [Twain.Middleware]
routes conn =
    [ Twain.get "/" $ Twain.send $ Twain.text "hi"
    , Twain.get "/categories" $ do
        -- http -v localhost:8080/categories
        categories <- liftIO $ getCategories conn
        Twain.send $ Twain.json categories
    , Twain.get "/transactions" $ do
        -- http -v localhost:8080/transactions/ accountId==2
        accountId <- Twain.queryParam "accountId"
        transactions <- liftIO $ getTransactionsWithRunningBalance conn (MkAccountId accountId)
        -- Twain.send $ Twain.json $ map toWithRunningBalanceDTO transactions
        Twain.send $ Twain.json transactions
    , Twain.get "/echo/:name" echoName
    , Twain.get "/greet/:name" $ do
        name <- Twain.param "name"
        Twain.send $ greeting name
    , Twain.get "/div/:num" $ do
        n :: Int <- read <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n `div` 2)
    , Twain.get "/inc/:num" $ do
        n :: Int <- maybe (error "oops") id . readMaybe <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n + 1)
    , Twain.get "/dec/:num" $ do
        n :: Int <- Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n - 1)
    , Twain.get "/dbl/:num" $ do
        num :: Int <- Twain.param "num"

        -- num :: Maybe Int <- readMaybe <$> Twain.param "num"
        -- num :: Int <- maybe (error "oops") id <$> paramMaybe "num"
        -- neg :: Maybe Bool <- queryParamMaybe "neg"
        -- liftIO $ putStrLn $ "Calling hello1: " <> hello1
        -- liftIO $ putStrLn $ "Calling hello2: " <> hello2
        -- void $ error "error!!"
        -- case num of
        --     Nothing -> Twain.send $ Twain.status status500 $ Twain.text "Invalid number format"
        --     Just n ->

        Twain.send
            . Twain.text
            . T.pack
            . show
            $ num * 2
    ]

data Category = MkCategory
    { categoryId :: Int
    , categoryName :: String
    , categoryCreatedAt :: Int
    , categoryUpdatedAt :: Int
    }
    deriving (Generic, Show)
    deriving
        (Elm, ToJSON, FromJSON)
        via ElmStreet Category

instance FromRow Category where
    fromRow =
        MkCategory
            <$> field
            <*> field
            <*> field
            <*> field

-- instance ToJSON Category where
--     toJSON (MkCategory{categoryId = categoryId', categoryName = categoryName', categoryCreatedAt = categoryCreatedAt', categoryUpdatedAt = categoryUpdatedAt'}) =
--         object
--             [ ("categoryId", toJSON categoryId')
--             , ("name", toJSON categoryName')
--             , ("createdAt", toJSON categoryCreatedAt')
--             , ("updatedAt", toJSON categoryUpdatedAt')
--             ]

getCategories :: Connection -> IO [Category]
getCategories conn =
    query_ conn sql
  where
    sql =
        [r|
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
|]

{-

Represents the ledger entries from an account's point of view.

Most of the times, we want to observer the list of transactions for a given
account (let's say "Checking account").

And we also want to see how each transaction affected the balance of that
account, so we show a running balance on each row.

 -}
data WithRunningBalanceEntity = MkWithRunningBalanceEntity
    { wrbeTransactionId :: Int
    , wrbeFromAccountId :: Int
    , wrbeFromAccountName :: String
    , wrbeToAccountId :: Int
    , wrbeToAccountName :: String
    , wrbeDateUnix :: Int
    , wrbeDate :: String
    , wrbeDescr :: String
    , wrbeFlowCents :: Int
    , wrbeFlow :: String
    , wrbeRunningBalanceCents :: Int
    , wrbeRunningBalance :: String
    , wrbeCreatedAtUnix :: Int
    , wrbeCreatedAtUtc :: String
    , wrbeCreatedAtTz :: String
    , wrbeUpdatedAtUnix :: Int
    , wrbeUpdatedAtUtc :: String
    , wrbeUpdatedAtTz :: String
    }
    deriving (Show, Generic)
    deriving
        (Elm, ToJSON, FromJSON)
        via ElmStreet WithRunningBalanceEntity

instance FromRow WithRunningBalanceEntity where
    fromRow =
        MkWithRunningBalanceEntity
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field
            <*> field

-- data WithRunningBalanceDTO = MkWithRunningBalanceDTO
--     { transactionId :: Int
--     , fromAccountId :: Int
--     , fromAccountName :: String
--     , toAccountId :: Int
--     , toAccountName :: String
--     , date :: Int
--     , dateFmt :: UTCTime
--     , descr :: String
--     , flowCents :: Int
--     , flow :: String
--     , runningBalanceCents :: Int
--     , runningBalance :: String
--     }
--     deriving (Show)

-- toWithRunningBalanceDTO :: WithRunningBalanceEntity -> WithRunningBalanceDTO
-- toWithRunningBalanceDTO (MkWithRunningBalanceEntity{..}) =
--     MkWithRunningBalanceDTO
--         { transactionId = transactionId
--         , fromAccountName = fromAccountName
--         , toAccountName = toAccountName
--         , date = date
--         , dateFmt = posixSecondsToUTCTime $ fromIntegral date
--         , descr = descr
--         , flowCents = flowCents
--         , flow = formatCents flowCents
--         , runningBalanceCents = runningBalanceCents
--         , runningBalance = formatCents runningBalanceCents
--         }
--   where
--     formatCents :: Int -> String
--     formatCents n = printf "%0.2f" (fromIntegral n / 100.0 :: Double)

-- formatDate :: Int -> Date
-- formatDate n = posixSecondsToUTCTime n

-- instance ToJSON WithRunningBalanceEntity where
--     toJSON
--         (MkWithRunningBalanceEntity{..}) =
--             object
--                 [ ("transactionId", toJSON transactionId)
--                 , ("fromAccountId", toJSON fromAccountId)
--                 , ("fromAccountName", toJSON fromAccountName)
--                 , ("toAccountId", toJSON toAccountId)
--                 , ("toAccountName", toJSON toAccountName)
--                 , ("dateUnix", toJSON dateUnix)
--                 , ("date", toJSON date)
--                 , ("descr", toJSON descr)
--                 , ("flowCents", toJSON flowCents)
--                 , ("flow", toJSON flow)
--                 , ("runningBalanceCents", toJSON runningBalanceCents)
--                 , ("runningBalance", toJSON runningBalance)
--                 , ("createdAtUnix", toJSON createdAtUnix)
--                 , ("createdAtUtc", toJSON createdAtUtc)
--                 , ("createdAtTz", toJSON createdAtTz)
--                 , ("updatedAtUnix", toJSON updatedAtUnix)
--                 , ("updatedAtUtc", toJSON updatedAtUtc)
--                 , ("updatedAtTz", toJSON updatedAtTz)
--                 ]

newtype AccountId = MkAccountId Int

getTransactionsWithRunningBalance :: Connection -> AccountId -> IO [WithRunningBalanceEntity]
getTransactionsWithRunningBalance conn (MkAccountId accountId) =
    query
        conn
        sql
        (accountId, accountId, accountId)
  where
    sql =
        [r|
SELECT y.transaction_id
     , y.from_account_id
     , y.from_account_name
     , y.to_account_id
     , y.to_account_name
     , y.date AS date_unix
     , date(y.date, 'unixepoch') AS date
     , y.descr
     , y.flow_cents
     , printf("%.2f", y.flow_cents / 100.0) AS flow
     , y.running_balance_cents
     , printf("%.2f", y.running_balance_cents / 100.0) AS running_balance
     , y.created_at AS created_at_unix
     , datetime(y.created_at, 'unixepoch') AS created_at_utc
     , datetime(y.created_at, 'unixepoch', 'localtime') AS created_at_tz
     , y.updated_at AS updated_at_unix
     , datetime(y.updated_at, 'unixepoch') AS updated_at_utc
     , datetime(y.updated_at, 'unixepoch', 'localtime') AS updated_at_tz
FROM (
        SELECT x.*
      , SUM(x.flow_cents) OVER (ORDER BY x.transaction_id) AS running_balance_cents
        FROM (
        SELECT t.transaction_id
                , a.name AS from_account_name
                , a.account_id AS from_account_id
                , b.name AS to_account_name
                , b.account_id AS to_account_id
                , t.date
                , t.descr
                , t.created_at
                , t.updated_at
                , t.cents * CASE WHEN t.from_account_id = ? THEN -1 ELSE 1 END AS flow_cents
        FROM transactions AS t

        INNER JOIN accounts AS a
                ON t.from_account_id = a.account_id

        INNER JOIN accounts AS b
                ON t.to_account_id = b.account_id

        WHERE t.to_account_id = ? OR t.from_account_id = ?
        )x
)y

ORDER BY y.transaction_id
;
|]

runServer :: Port -> IO ()
runServer port = do
    conn <- open "./db.sqlite3"
    categories <- getCategories conn
    mapM_ print categories
    transactions <- getTransactionsWithRunningBalance conn (MkAccountId 2)
    mapM_ print transactions
    putStrLn $
        unwords
            [ "Running twain app at"
            , "http://localhost:" <> show port
            , "(ctrl-c to quit)"
            ]
    run port $ mkApp conn
