module Server.DB.Budgets.Queries
  ( BudgetDB(..)
  , getAllBudgets
  , getBudgetById
  , getBudgetIdForDate
  , insertBudgetForDate
  ) where

import Prelude

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names
newtype BudgetRow = BudgetRow
  { budget_id :: Int
  , starts_on :: Int
  , ends_on :: Int
  , created_at :: Int
  , updated_at :: Int
  }

derive instance Generic BudgetRow _
derive newtype instance ReadForeign BudgetRow

-- | Row type for budget ID queries
newtype BudgetIdRow = BudgetIdRow { budget_id :: Int }

derive instance Generic BudgetIdRow _
derive newtype instance ReadForeign BudgetIdRow

-- | Domain type with proper PureScript naming
newtype BudgetDB = BudgetDB
  { budgetId :: Int
  , startsOnUnix :: Int
  , endsOnUnix :: Int
  , createdAtUnix :: Int
  , updatedAtUnix :: Int
  }

derive instance Generic BudgetDB _
derive newtype instance Show BudgetDB
derive newtype instance Eq BudgetDB
derive newtype instance WriteForeign BudgetDB
derive newtype instance ReadForeign BudgetDB

-- | Convert database row to domain type
rowToBudget :: BudgetRow -> BudgetDB
rowToBudget (BudgetRow row) = BudgetDB
  { budgetId: row.budget_id
  , startsOnUnix: row.starts_on
  , endsOnUnix: row.ends_on
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

-- | Get all budgets
getAllBudgets :: SQLite3.DBConnection -> Aff (Array BudgetDB)
getAllBudgets db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Budgets/sql/getAllBudgets.sql"
  rows <- SQLite3.queryDB db sql []
  fromDbRows "budgets" rowToBudget rows

-- | Get budget by ID
getBudgetById :: Int -> SQLite3.DBConnection -> Aff (Maybe BudgetDB)
getBudgetById budgetId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Budgets/sql/getBudgetById.sql"
  rows <- SQLite3.queryDB db sql [unsafeToForeign budgetId]
  budgets <- fromDbRows "budgets" rowToBudget rows
  pure $ head budgets

-- | Get budget ID for a date (returns Nothing if not found)
getBudgetIdForDate :: Int -> SQLite3.DBConnection -> Aff (Maybe Int)
getBudgetIdForDate dateUnix db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Budgets/sql/getBudgetIdForDate.sql"
  rows <- SQLite3.queryDB db sql [unsafeToForeign dateUnix, unsafeToForeign dateUnix]
  budgetIds <- fromDbRows "budget_ids" (\(BudgetIdRow row) -> row.budget_id) rows
  pure $ head budgetIds

-- | Insert a budget for a date (monthly budget)
insertBudgetForDate :: Int -> SQLite3.DBConnection -> Aff Int
insertBudgetForDate dateUnix db = do
  -- Create a monthly budget period
  let startsOnUnix = dateUnix
  let endsOnUnix = dateUnix + (30 * 24 * 60 * 60) -- 30 days later

  liftEffect $ log $ "Creating budget: starts_on=" <> show startsOnUnix <> ", ends_on=" <> show endsOnUnix

  insertSql <- FS.readTextFile UTF8 "src/Server/DB/Budgets/sql/insertBudget.sql"
  _ <- SQLite3.queryDB db insertSql [unsafeToForeign startsOnUnix, unsafeToForeign endsOnUnix]

  -- Get the inserted budget ID
  lastIdSql <- FS.readTextFile UTF8 "src/Server/DB/Budgets/sql/getLastInsertRowId.sql"
  rows <- SQLite3.queryDB db lastIdSql []
  budgetIds <- fromDbRows "budget_ids" (\(BudgetIdRow row) -> row.budget_id) rows
  case head budgetIds of
    Nothing -> pure 1 -- fallback
    Just budgetId -> pure budgetId