{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.User
  ( UserDTO
  , fromUser
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Domain.User (User (..))
import Elm
import GHC.Generics (Generic)

data UserDTO = MkUserDTO
  { userId :: Int
  , fullName :: Text
  , email :: Text
  }
  deriving stock (Generic)

instance Elm UserDTO
instance ToJSON UserDTO
instance FromJSON UserDTO

fromUser :: User -> UserDTO
fromUser MkUser{..} =
  MkUserDTO
    { userId = userId
    , fullName = fullName
    , email = email
    }
