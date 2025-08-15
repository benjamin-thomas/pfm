module WIP where

import Prelude

import Data.Array as Array
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (logShow)
import SQLite3 as SQLite3
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Transactions.Queries as TransactionQueries
import Server.Database (seedFromOfx)

{-

Usage
=====

repl> launchAff_ $ newConn >>= DB.getSomething
repl> newConn >>= DB.getSomething # launchAff_

---

repl> import Server.DB.LedgerView.Queries as LedgerViewQueries
repl> launchAff_ $ newConn >>= LedgerViewQueries.getLedgerViewRows 4 <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow

---

-- launchAff_ $ newConn >>= DB.getAllUsers
-- launchAff_ $ newConn >>= DB.seedFromOfx ".tmp/CA20250630_124433.ofx"
-- launchAff_ $ newConn >>= DB.getBudgetIdForDate 1719792000
-- launchAff_ $ newConn >>= DB.getAllTransactions >>= traverse_ logShow
-- launchAff_ $ newConn >>= DB.getAllTransactions <#> (_ !! 5) >>= traverse_ logShow
-- launchAff_ $ newConn >>= DB.getAllTransactions <#> Array.take 3 >>= traverse_ logShow
-- launchAff_ $ newConn >>= DB.getAllTransactions <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow
-- launchAff_ $ newConn >>= DB.getLedgerViewRows <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow

 -}

-- Database connection with foreign keys enabled
newConn :: Aff SQLite3.DBConnection
newConn = do
  conn <- SQLite3.newDB "./db.sqlite"
  _ <- SQLite3.queryDB conn "PRAGMA foreign_keys = ON" []
  pure conn

{-

Via the REPL:
  repl> seed

Run as a script (faster log feedback)
  node -e "import('./output/WIP/index.js').then(m => m.seed())"

 -}
seed :: Effect Unit
seed = launchAff_ $ newConn >>= seedFromOfx ".tmp/CA20250630_124433.ofx"

showTransactions :: Effect Unit
showTransactions =
  launchAff_ $ newConn >>= TransactionQueries.getAllTransactions >>= traverse_ logShow

-- launchAff_ wip
wip ∷ Aff Unit
wip = newConn >>= LedgerViewQueries.getLedgerViewRows 4 <#> Array.drop 3 <#> Array.take 3 >>= traverse_ logShow

