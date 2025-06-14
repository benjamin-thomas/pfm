{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module DTO.User
  ( UserDTO
  , toUserDTO
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Domain.User qualified as Domain
import Elm
import GHC.Generics (Generic)

data UserDTO = MkUserDTO
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  }
  deriving stock (Generic)

instance Elm UserDTO
instance ToJSON UserDTO
instance FromJSON UserDTO

toUserDTO :: Domain.User -> UserDTO
toUserDTO user =
  MkUserDTO
    { userId = Domain.userId user
    , userName = Domain.userName user
    , userEmail = Domain.userEmail user
    }
