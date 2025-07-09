{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DB.Budgets.JSON where

import DB.Budgets.Queries
import DTO.Utils (dropAndLowerHead, fromUTC)
import Data.Aeson
import Elm (Elm)
import GHC.Generics (Generic)

data BudgetJSON = MkBudgetJSON
    { budgetId :: Int
    , budgetStartsOn :: Int
    , budgetEndsOn :: Int
    , budgetCreatedAt :: Int
    , budgetUpdatedAt :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (Elm)

instance ToJSON BudgetJSON where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "budget")
                }

{-

ghci> :m + DB.Budgets.Queries DB.Budgets.JSON Data.Functor
ghci> _newConn >>= getAll
ghci> _newConn >>= getAll <&> map fromBudgetDB

 -}
fromBudgetDB :: BudgetDB -> BudgetJSON
fromBudgetDB MkBudgetDB{..} =
    MkBudgetJSON
        { budgetId = budgetId
        , budgetStartsOn = fromUTC budgetStartsOn
        , budgetEndsOn = fromUTC budgetEndsOn
        , budgetCreatedAt = fromUTC budgetCreatedAt
        , budgetUpdatedAt = fromUTC budgetUpdatedAt
        }
