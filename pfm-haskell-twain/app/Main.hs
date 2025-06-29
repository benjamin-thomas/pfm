{-# LANGUAGE OverloadedStrings #-}

module Main where

import ElmExport (exportElm)
import Server qualified

{-

rg --files | entr -rc cabal run

 -}
main :: IO ()
main =
    do
        exportElm
        Server.runServer 8080
