module Main where

import Server qualified

{-

rg --files | entr -rc cabal run

 -}
main :: IO ()
main =
    do
        Server.exportElm
        Server.runServer 8080
