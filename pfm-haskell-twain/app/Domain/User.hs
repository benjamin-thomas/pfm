{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Domain.User
  ( User (..)
  , fromUserRow
  , isOnNewPlatform
  ) where

import DB.User (UserRow (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data User = MkUser
  { userId :: Int
  , fullName :: Text
  , email :: Text
  , joinedOn :: UTCTime
  }

fromUserRow :: UserRow -> User
fromUserRow MkUserRow{..} =
  MkUser
    { userId = userId
    , fullName =
        T.unwords
          [ firstName
          , lastName
          ]
    , email = email
    , joinedOn = posixSecondsToUTCTime $ fromIntegral createdAt
    }

-- if joined after 2025-06-01, then they are on the new platform
isOnNewPlatform :: User -> Bool
isOnNewPlatform MkUser{..} =
  joinedOn > UTCTime (fromGregorian 2025 6 1) 0