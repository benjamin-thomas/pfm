{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.Category
  ( Category
  , fromCategoryRow
  , fmtCategory
  ) where

import Data.Aeson (FromJSON, ToJSON)
import DB.Category (CategoryRow(..))
import Elm
import GHC.Generics (Generic)

data Category = MkCategory
  { categoryId :: Int
  , categoryName :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (Elm, ToJSON, FromJSON)

fromCategoryRow :: CategoryRow -> Category
fromCategoryRow MkCategoryRow{..} =
  MkCategory
    { categoryId = categoryRowId
    , categoryName = categoryRowName
    }

{- | Format a category for display with additional context
-}
fmtCategory :: Category -> String
fmtCategory category =
  mconcat
    [ "#" <> show (categoryId category)
    , ": "
    , categoryName category
    ]
