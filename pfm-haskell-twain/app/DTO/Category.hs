{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.Category
  ( Category
  , fromCategoryRow
  , fmtCategory
  ) where

import DB.Category (CategoryRow (..))
import DTO.Utils (dropAndLowerHead)
import Data.Aeson
import Elm
import GHC.Generics (Generic)

data Category = MkCategory
  { categoryId :: Int
  , categoryName :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (Elm, FromJSON)

instance ToJSON Category where
  toJSON =
    genericToJSON
      defaultOptions
        { fieldLabelModifier = dropAndLowerHead (length "category")
        }

fromCategoryRow :: CategoryRow -> Category
fromCategoryRow MkCategoryRow{..} =
  MkCategory
    { categoryId = categoryRowId
    , categoryName = categoryRowName
    }

fmtCategory :: Category -> String
fmtCategory category =
  mconcat
    [ "#" <> show (categoryId category)
    , ": "
    , categoryName category
    ]
