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
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import SQLite3 as SQLite3
import Yoga.JSON (class ReadForeign, class WriteForeign)

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

-- | Get all budgets
getAllBudgets :: SQLite3.DBConnection -> Aff (Array BudgetDB)
getAllBudgets db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets ORDER BY starts_on DESC" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToBudget rowArray
  where
  rowToBudget :: Foreign -> BudgetDB
  rowToBudget row =
    let
      obj = unsafeFromForeign row :: { budget_id :: Int, starts_on :: Int, ends_on :: Int, created_at :: Int, updated_at :: Int }
    in
      BudgetDB { budgetId: obj.budget_id, startsOnUnix: obj.starts_on, endsOnUnix: obj.ends_on, createdAtUnix: obj.created_at, updatedAtUnix: obj.updated_at }

-- | Get budget by ID
getBudgetById :: Int -> SQLite3.DBConnection -> Aff (Maybe BudgetDB)
getBudgetById budgetId db = do
  rows <- SQLite3.queryDB db "SELECT budget_id, starts_on, ends_on, created_at, updated_at FROM budgets WHERE budget_id = ?" [ unsafeToForeign budgetId ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int, starts_on :: Int, ends_on :: Int, created_at :: Int, updated_at :: Int }
      pure $ Just $ BudgetDB { budgetId: obj.budget_id, startsOnUnix: obj.starts_on, endsOnUnix: obj.ends_on, createdAtUnix: obj.created_at, updatedAtUnix: obj.updated_at }

-- | Get budget ID for a date (returns Nothing if not found)
getBudgetIdForDate :: Int -> SQLite3.DBConnection -> Aff (Maybe Int)
getBudgetIdForDate dateUnix db = do
  rows <- SQLite3.queryDB db
    "SELECT budget_id FROM budgets WHERE ? >= starts_on AND ? < ends_on"
    [ unsafeToForeign dateUnix, unsafeToForeign dateUnix ]
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure Nothing
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int }
      pure $ Just obj.budget_id

-- | Insert a budget for a date (monthly budget)
insertBudgetForDate :: Int -> SQLite3.DBConnection -> Aff Int
insertBudgetForDate dateUnix db = do
  -- Create a monthly budget period
  let startsOnUnix = dateUnix
  let endsOnUnix = dateUnix + (30 * 24 * 60 * 60) -- 30 days later

  liftEffect $ log $ "Creating budget: starts_on=" <> show startsOnUnix <> ", ends_on=" <> show endsOnUnix

  _ <- SQLite3.queryDB db
    "INSERT INTO budgets (starts_on, ends_on) VALUES (?, ?)"
    [ unsafeToForeign startsOnUnix, unsafeToForeign endsOnUnix ]

  -- Get the inserted budget ID
  rows <- SQLite3.queryDB db "SELECT last_insert_rowid() as budget_id" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure 1 -- fallback
    Just row -> do
      let obj = unsafeFromForeign row :: { budget_id :: Int }
      pure obj.budget_id