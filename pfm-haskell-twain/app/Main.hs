{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.SQLite.Simple (Connection, execute_, open)
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

{-

Temporary, for GHCi exploration.

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m +Database.Category Domain.Category
ghci> categories <- getCategories =<< newConn
ghci> map fmtCategory categories
ghci> mapM isStale categories

 -}
newConn :: IO Connection
newConn = do
    conn <- open "./db.sqlite3"
    execute_ conn "PRAGMA foreign_keys = ON"
    pure conn