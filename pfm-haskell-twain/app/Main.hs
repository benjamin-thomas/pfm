module Main where

import Server qualified
import Server2 qualified

{-

rg --files | entr -rc cabal run

 -}
main :: IO ()
main =
    if False
        then
            Server2.start 8080
        else
            Server.runServer 8080
