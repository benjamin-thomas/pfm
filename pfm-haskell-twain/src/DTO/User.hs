{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module DTO.User
  ( User
  , fromUserRow
  ) where

import DB.User.Queries (UserRow (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Elm
import GHC.Generics (Generic)

data User = MkUser
  { userId :: Int
  , userFullName :: Text
  , userEmail :: Text
  , userJoinedOn :: UTCTime
  }
  deriving stock (Generic)

instance Elm User
instance ToJSON User
instance FromJSON User

fromUserRow :: UserRow -> User
fromUserRow MkUserRow{..} =
  MkUser
    { userId = userId
    , userFullName = T.unwords [firstName, lastName]
    , userEmail = email
    , userJoinedOn = posixSecondsToUTCTime $ fromIntegral createdAt
    }
