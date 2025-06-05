{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Control.Monad
import qualified Data.Text as T
import Network.HTTP.Types (status500)
import Network.Wai.Handler.Warp (Port, run)
import Text.Read (readMaybe)
import qualified Web.Twain as Twain

mkApp :: Twain.Application
mkApp =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        routes

hello1 :: String
hello1 = "hello1"

hello2 :: String
hello2 = error "oops"

routes :: [Twain.Middleware]
routes =
    [ Twain.get "/" $ Twain.send $ Twain.text "hi"
    , Twain.get "/div/:num" $ do
        n :: Int <- read <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n `div` 2)
    , Twain.get "/inc/:num" $ do
        n :: Int <- maybe (error "oops") id . readMaybe <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (n + 1)
    , Twain.get "/dbl/:num" $ do
        num :: Maybe Int <- readMaybe <$> Twain.param "num"
        -- liftIO $ putStrLn $ "Calling hello1: " <> hello1
        -- liftIO $ putStrLn $ "Calling hello2: " <> hello2
        void $ error "error!!"
        case num of
            Nothing -> Twain.send $ Twain.status status500 $ Twain.text "Invalid number format"
            Just n -> Twain.send $ Twain.text $ T.pack $ show (n * 2)
    ]

runServer :: Port -> IO ()
runServer port = do
    putStrLn $
        unwords
            [ "Running twain app at"
            , "http://localhost:" <> show port
            , "(ctrl-c to quit)"
            ]
    run port mkApp