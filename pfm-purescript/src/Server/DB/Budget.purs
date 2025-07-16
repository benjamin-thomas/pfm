module Server.DB.Budget
  ( BudgetDB(..)
  , getAllBudgets
  , getBudgetById
  , getBudgetIdForDate
  , insertBudgetForDate
  ) where

import Prelude

import Data.Array (head)
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Server.DB.Utils (fromDbRows)
import Yoga.JSON as JSON
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

-- | Database row type matching SQL column names  
newtype BudgetRow = MkBudgetRow
  { budget_id :: Int
  , starts_on :: Int
  , ends_on :: Int
  , created_at :: Int
  , updated_at :: Int
  }

derive instance Generic BudgetRow _
derive newtype instance ReadForeign BudgetRow

-- | Row type for budget ID queries
newtype BudgetIdRow = MkBudgetIdRow { budget_id :: Int }

derive instance Generic BudgetIdRow _
derive newtype instance ReadForeign BudgetIdRow

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
rowToBudgetDB :: BudgetRow -> BudgetDB
rowToBudgetDB (MkBudgetRow row) = BudgetDB
  { budgetId: row.budget_id
  , startsOnUnix: row.starts_on
  , endsOnUnix: row.ends_on
  , createdAtUnix: row.created_at
  , updatedAtUnix: row.updated_at
  }

-- | Get all budgets
getAllBudgets :: SQLite3.DBConnection -> Aff (Array BudgetDB)
getAllBudgets db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets ORDER BY starts_on DESC" []
  fromDbRows "budgets" rowToBudgetDB rows

-- | Get budget by ID
getBudgetById :: Int -> SQLite3.DBConnection -> Aff (Maybe BudgetDB)
getBudgetById budgetId db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets WHERE budget_id = ?" [ JSON.writeImpl budgetId ]
  budgets <- fromDbRows "budgets" rowToBudgetDB rows
  pure $ head budgets

-- | Get budget ID for a date (returns Nothing if not found)
getBudgetIdForDate :: Int -> SQLite3.DBConnection -> Aff (Maybe Int)
getBudgetIdForDate dateUnix db = do
  rows <- SQLite3.queryDB db
    "SELECT budget_id FROM budgets WHERE ? >= starts_on AND ? < ends_on"
    [ JSON.writeImpl dateUnix, JSON.writeImpl dateUnix ]
  budgetIds <- fromDbRows "budget_ids" (\(MkBudgetIdRow row) -> row.budget_id) rows
  pure $ head budgetIds

-- | Insert a budget for a date (monthly budget)
insertBudgetForDate :: Int -> SQLite3.DBConnection -> Aff Int
insertBudgetForDate dateUnix db = do
  -- Create a monthly budget period
  let startsOnUnix = dateUnix
  let endsOnUnix = dateUnix + (30 * 24 * 60 * 60) -- 30 days later

  liftEffect $ log $ "Creating budget: starts_on=" <> show startsOnUnix <> ", ends_on=" <> show endsOnUnix

  _ <- SQLite3.queryDB db
    "INSERT INTO budgets (starts_on, ends_on) VALUES (?, ?)"
    [ JSON.writeImpl startsOnUnix, JSON.writeImpl endsOnUnix ]

  -- Get the inserted budget ID
  rows <- SQLite3.queryDB db "SELECT last_insert_rowid() as budget_id" []
  budgetIds <- fromDbRows "budget_ids" (\(MkBudgetIdRow row) -> row.budget_id) rows
  case head budgetIds of
    Nothing -> pure 1 -- fallback
    Just budgetId -> pure budgetId