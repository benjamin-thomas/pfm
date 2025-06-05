module Server2 (start) where

import Control.Monad (msum)
import Happstack.Server (Method (GET), ServerPartT, dir, dirs, lookRead, method, notFound, nullConf, ok, path, port, seeOther, simpleHTTP)

wow :: ServerPartT IO String
wow =
    msum
        [ dirs "hello/world" $ ok "Hello, World!"
        , dirs "goodbye/moon" $ ok "Goodbye, Moon!"
        , dir "hello" $ path $ \s -> ok $ "Hello, " ++ s
        , dir "inc" $ path $ \n -> ok $ show (n + 1 :: Int)
        , dir "add" $
            path $
                \a -> path $ \b -> ok $ show (a + b :: Int)
        , dir "adder" $ do
            a <- lookRead "a"
            b <- lookRead "b"
            ok $ show (a + b :: Int)
        , notFound "Nothing's here!"
        ]

start :: Int -> IO ()
start port' = simpleHTTP nullConf{port = port'} wow