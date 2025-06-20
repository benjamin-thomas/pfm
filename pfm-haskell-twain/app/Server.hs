{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (runServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Accounts.Queries qualified as AccountQueries
import DB.Category.Queries (CategoryRow, getNonStaleCategories)
import DB.LedgerView.Queries (AccountId (MkAccountId), LedgerViewRow, getLedgerViewRows)
import DB.Transactions.Queries
import DB.User.Queries
import DTO.AccountRead qualified as AccountRead
import DTO.Category (Category, fromCategoryRow)
import DTO.Ledger (LedgerLineSummary, fromLedgerViewRow)
import DTO.TransactionWrite (toTransactionNewRow)
import DTO.User (fromUserRow)
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, open)
import Network.HTTP.Types (status200, status201)
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Text.Read (readMaybe)
import Web.Twain qualified as Twain

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
            . Twain.withHeader
                ("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
            . Twain.withHeader
                ("Access-Control-Allow-Headers", "Content-Type")

echoName :: Twain.ResponderM ()
echoName = do
    name <- Twain.param "name"
    Twain.send $ Twain.html $ "Hello, " <> name

greeting :: ByteString -> Twain.Response
greeting name = Twain.html $ "Hello, " <> name

{- FOURMOLU_DISABLE -}
-- http -v localhost:8080/categories
handleCategories :: Connection -> Twain.ResponderM ()
handleCategories conn = do
    categoriesDb <- liftIO $ getNonStaleCategories conn :: Twain.ResponderM [CategoryRow]
    let categories = map fromCategoryRow categoriesDb  :: [Category]
    Twain.send $ Twain.json categories
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
-- http -v localhost:8080/transactions/ accountId==2
handleGetTransactions :: Connection -> Twain.ResponderM ()
handleGetTransactions conn = do
    accountId <- Twain.queryParam "accountId"
    transactions <- liftIO $ getLedgerViewRows (MkAccountId accountId) conn :: Twain.ResponderM [LedgerViewRow]
    let ledgerLineSummaries = map fromLedgerViewRow transactions            :: [LedgerLineSummary]
    Twain.send $ Twain.json ledgerLineSummaries
{- FOURMOLU_ENABLE -}

handlePostTransactions :: Connection -> Twain.ResponderM ()
handlePostTransactions conn = do
    toInsert <- toTransactionNewRow <$> Twain.fromBody
    liftIO $ insertTransaction conn toInsert
    -- FIXME: how should I handle the account context?
    transactions <- liftIO $ getLedgerViewRows (MkAccountId 2) conn :: Twain.ResponderM [LedgerViewRow]
    let ledgerLineSummaries = map fromLedgerViewRow transactions :: [LedgerLineSummary]
    Twain.send $ Twain.status status201 $ Twain.json ledgerLineSummaries

-- http -v localhost:8080/users all==1
handleUsers :: Connection -> Twain.ResponderM ()
handleUsers conn = do
    allP <-
        fmap
            (maybe False (("0" :: String) /=))
            (Twain.queryParamMaybe "all")

    liftIO $ putStrLn $ "\x1b[33mallP: " <> show allP <> "\x1b[0m"
    userRows <-
        let getUsers = if allP then getAllUserRows else getNewPlatformUserRows
         in liftIO $ getUsers conn
    let users = map fromUserRow userRows
    Twain.send $ Twain.json users

handleAccounts :: Connection -> Twain.ResponderM ()
handleAccounts conn = do
    accountRows <- liftIO $ AccountQueries.getAll conn
    let accounts_ = map AccountRead.fromAccountRow accountRows
    Twain.send $ Twain.json accounts_

routes :: Connection -> [Twain.Middleware]
routes conn =
    [ Twain.get "/" $ Twain.send $ Twain.text "hi"
    , Twain.get "/accounts" $ handleAccounts conn
    , Twain.get "/categories" $ handleCategories conn
    , Twain.get "/transactions" $ handleGetTransactions conn
    , Twain.post "/transactions" $ handlePostTransactions conn
    , Twain.route (Just "OPTIONS") "/transactions" $ Twain.send $ Twain.status status200 $ Twain.json ()
    , Twain.get "/users" $ handleUsers conn
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

runServer :: Port -> IO ()
runServer port = do
    conn <- open "./db.sqlite3"
    putStrLn $
        unwords
            [ "Running twain app at"
            , "http://localhost:" <> show port
            ]
    run port $ mkApp conn
