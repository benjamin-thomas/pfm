module WIP where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import SQLite3 as SQLite3
import Server.Database (seedFromOfx)

{-

Usage
=====

repl> run $ newConn >>= DB.getAllUsers
repl> newConn >>= DB.getAllUsers # run

---

run $ newConn >>= DB.getAllUsers
run $ newConn >>= DB.seedFromOfx ".tmp/CA20250630_124433.ofx"
run $ newConn >>= DB.getBudgetIdForDate 1719792000

 -}
run :: forall a. Show a => Aff a -> Effect Unit
run =
  runAff_
    ( either
        (\err -> log $ "Error: " <> show err)
        (log <<< show)
    )

-- Database connection with foreign keys enabled
newConn :: Aff SQLite3.DBConnection
newConn = do
  conn <- SQLite3.newDB "./db.sqlite"
  _ <- SQLite3.queryDB conn "PRAGMA foreign_keys = ON" []
  pure conn

-- Seed database from OFX file
seed :: Effect Unit
seed = run $ newConn >>= seedFromOfx ".tmp/CA20250630_124433.ofx"

main :: Effect Unit
main = seed