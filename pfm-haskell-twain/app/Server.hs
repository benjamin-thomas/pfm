{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import qualified Data.Text as T
import Network.Wai.Handler.Warp (Port, run)
import qualified Web.Twain as Twain

mkApp :: Twain.Application
mkApp =
    foldr
        ($)
        (Twain.notFound $ Twain.send $ Twain.text "Error: not found.")
        routes

routes :: [Twain.Middleware]
routes =
    [ Twain.get "/" $
        Twain.send $
            Twain.text "hi"
    , Twain.get "/dbl/:num" $ do
        num :: Int <- read <$> Twain.param "num"
        Twain.send $ Twain.text $ T.pack $ show (num * 2)
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