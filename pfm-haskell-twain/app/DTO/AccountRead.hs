{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.AccountRead (AccountRead, fromAccountRow) where

import DB.Accounts.Queries (AccountReadRow (..))
import DTO.Utils (dropAndLowerHead)
import Data.Aeson (Options (fieldLabelModifier), ToJSON, defaultOptions, genericToJSON)
import Data.Aeson.Types (toJSON)
import Data.Text (Text)
import Elm (Elm)
import GHC.Generics (Generic)

data AccountRead = MkAccountRead
    { accountReadAccountId :: Int
    , accountReadCategoryId :: Int
    , accountReadCategoryName :: Text
    , accountReadName :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (Elm)

instance ToJSON AccountRead where
    toJSON =
        genericToJSON
            defaultOptions
                { fieldLabelModifier = dropAndLowerHead (length "accountRead")
                }

fromAccountRow :: AccountReadRow -> AccountRead
fromAccountRow MkAccountReadRow{..} =
    MkAccountRead
        { accountReadAccountId = accountId
        , accountReadCategoryId = categoryId
        , accountReadCategoryName = categoryName
        , accountReadName = name
        }
