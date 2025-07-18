module WIP where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (either)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log, logShow)
import SQLite3 as SQLite3
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Transactions.Queries (TransactionDB(..))
import Server.Database (seedFromOfx)
import Server.Database as DB
import Server.DB.Transactions.Queries as TransactionQueries
import Shared.Types (Transaction(..))

{-

Usage
=====

repl> run $ newConn >>= DB.getSomething
repl> newConn >>= DB.getSomething # run

---

-- run $ newConn >>= DB.getAllUsers
-- run $ newConn >>= DB.seedFromOfx ".tmp/CA20250630_124433.ofx"
-- run $ newConn >>= DB.getBudgetIdForDate 1719792000
-- run $ newConn >>= DB.getAllTransactions >>= traverse_ logShow
-- run $ newConn >>= DB.getAllTransactions <#> (_ !! 5) >>= traverse_ logShow
-- run $ newConn >>= DB.getAllTransactions <#> Array.take 3 >>= traverse_ logShow
-- run $ newConn >>= DB.getAllTransactions <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow
-- run $ newConn >>= DB.getLedgerViewRows <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow

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

showTransactions :: Effect Unit
showTransactions =
  run $ newConn >>= TransactionQueries.getAllTransactions >>= traverse_ logShow

wip = newConn >>= LedgerViewQueries.getLedgerViewRows 4 <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow
