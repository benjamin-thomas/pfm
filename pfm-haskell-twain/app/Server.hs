{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON), object)
import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as T
import Database.SQLite.Simple (Connection, FromRow, field, fromRow, open, query, query_)
import Network.Wai.Handler.Warp (Port, run)
import Text.Read (readMaybe)
import Web.Twain qualified as Twain

mkApp :: Connection -> Twain.Application
mkApp conn =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        (routes conn)

hello1 :: String
hello1 = "hello1"

hello2 :: String
hello2 = error "oops"

echoName :: Twain.ResponderM ()
echoName = do
    name <- Twain.param "name"
    Twain.send $ Twain.html $ "Hello, " <> name

echoName2 :: Twain.ResponderM a
echoName2 = do
    Twain.send $ Twain.html $ "Hello, " <> "Ben"

greeting :: ByteString -> Twain.Response
greeting name = Twain.html $ "Hello, " <> name

wat :: Twain.Middleware
wat = Twain.get "/echo/:name" echoName

routes :: Connection -> [Twain.Middleware]
routes conn =
    [ Twain.get "/" $ Twain.send $ Twain.text "hi"
    , Twain.get "/categories" $ do
        categories <- liftIO $ getCategories conn
        Twain.send $ Twain.json categories
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
getCategories conn = query_ conn "SELECT * FROM category"

runServer :: Port -> IO ()
runServer port = do
    conn <- open "./db.sqlite3"
    categories <- getCategories conn
    mapM_ print categories
    putStrLn $
        unwords
            [ "Running twain app at"
            , "http://localhost:" <> show port
            , "(ctrl-c to quit)"
            ]
    run port $ mkApp conn