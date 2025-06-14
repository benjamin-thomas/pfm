{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module DTO.Category
  ( CategoryDTO
  , toCategoryDTO
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Domain.Category (Category (..))
import Elm
import GHC.Generics (Generic)

data CategoryDTO = MkCategoryDTO
  { categoryDtoId :: Int
  , categoryDtoName :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (Elm, ToJSON, FromJSON)

toCategoryDTO :: Category -> CategoryDTO
toCategoryDTO category =
  MkCategoryDTO
    { categoryDtoId = categoryId category
    , categoryDtoName = categoryName category
    }
