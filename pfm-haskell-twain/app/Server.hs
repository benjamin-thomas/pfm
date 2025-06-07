{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (runServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON), object)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, FromRow, field, fromRow, open, query, query_)
import Network.Wai.Handler.Warp (Port, run)
import Text.Printf (printf)
import Text.RawString.QQ (r)
import Text.Read (readMaybe)
import Web.Twain qualified as Twain

mkApp :: Connection -> Twain.Application
mkApp conn =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        (routes conn)

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
    , Twain.get "/transactions/:accountId" $ do
        -- http -v localhost:8080/transactions/ accountId==2
        accountId <- Twain.queryParam "accountId"
        transactions <- liftIO $ getTransactionsWithRunningBalance conn (MkAccountId accountId)
        Twain.send $ Twain.json $ map toWithRunningBalanceDTO transactions
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
    }
    deriving (Show)

instance FromRow Category where
    fromRow =
        MkCategory
            <$> field
            <*> field

instance ToJSON Category where
    toJSON (MkCategory{categoryId = categoryId', categoryName = categoryName'}) =
        object
            [ ("categoryId", toJSON categoryId')
            , ("name", toJSON categoryName')
            ]

getCategories :: Connection -> IO [Category]
getCategories conn = query_ conn "SELECT * FROM categories"

{-

Represents the ledger entries from an account's point of view.

Most of the times, we want to observer the list of transactions for a given
account (let's say "Checking account").

And we also want to see how each transaction affected the balance of that
account, so we show a running balance on each row.

 -}
data WithRunningBalanceEntity = MkWithRunningBalanceEntity
    { transactionId :: Int
    , fromAccountName :: String
    , toAccountName :: String
    , date :: Int
    , descr :: String
    , flowCents :: Int
    , runningBalanceCents :: Int
    }
    deriving (Show)

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

data WithRunningBalanceDTO = MkWithRunningBalanceDTO
    { transactionId :: Int
    , fromAccountName :: String
    , toAccountName :: String
    , date :: Int
    , descr :: String
    , flowCents :: Int
    , flow :: String
    , runningBalanceCents :: Int
    , runningBalance :: String
    }
    deriving (Show)

toWithRunningBalanceDTO :: WithRunningBalanceEntity -> WithRunningBalanceDTO
toWithRunningBalanceDTO (MkWithRunningBalanceEntity{..}) =
    MkWithRunningBalanceDTO
        { transactionId = transactionId
        , fromAccountName = fromAccountName
        , toAccountName = toAccountName
        , date = date
        , descr = descr
        , flowCents = flowCents
        , flow = formatCents flowCents
        , runningBalanceCents = runningBalanceCents
        , runningBalance = formatCents runningBalanceCents
        }
  where
    formatCents :: Int -> String
    formatCents n = printf "%0.2f" (fromIntegral n / 100.0 :: Double)

instance ToJSON WithRunningBalanceDTO where
    toJSON
        (MkWithRunningBalanceDTO{..}) =
            object
                [ ("transactionId", toJSON transactionId)
                , ("fromAccountName", toJSON fromAccountName)
                , ("toAccountName", toJSON toAccountName)
                , ("date", toJSON date)
                , ("descr", toJSON descr)
                , ("flowCents", toJSON flowCents)
                , ("flow", toJSON flow)
                , ("runningBalanceCents", toJSON runningBalanceCents)
                , ("runningBalance", toJSON runningBalance)
                ]

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
SELECT x.*
     , SUM(x.flow_cents) OVER (ORDER BY x.transaction_id) AS running_balance_cents
FROM (
    SELECT t.transaction_id
         , a.name AS from_account
         , b.name AS to_account
         , t.date
         , t.descr
         , t.cents * CASE WHEN t.from_account_id = ? THEN -1 ELSE 1 END AS flow_cents
    FROM transactions AS t

    INNER JOIN accounts AS a
            ON t.from_account_id = a.account_id

    INNER JOIN accounts AS b
            ON t.to_account_id = b.account_id

    WHERE t.to_account_id = ? OR t.from_account_id = ?
)x

ORDER BY x.transaction_id
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