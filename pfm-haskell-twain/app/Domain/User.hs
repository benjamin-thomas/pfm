module Domain.User
  ( User
      ( MkUser
      , userId
      , userName
      , userEmail
      , userCreatedAt
      , userUpdatedAt
      )
  , fromUserRow
  ) where

import DB.User (UserRow (..))
import Data.Text (Text)

data User = MkUser
  { userId :: Int
  , userName :: Text
  , userEmail :: Text
  , userCreatedAt :: Int
  , userUpdatedAt :: Int
  }

fromUserRow :: UserRow -> User
fromUserRow
  ( MkUserRow
      { userRowId = userId'
      , userRowName = userName'
      , userRowEmail = userEmail'
      , userRowCreatedAt = userCreatedAt'
      , userRowUpdatedAt = userUpdatedAt'
      }
    ) =
    MkUser
      { userId = userId'
      , userName = userName'
      , userEmail = userEmail'
      , userCreatedAt = userCreatedAt'
      , userUpdatedAt = userUpdatedAt'
      }
