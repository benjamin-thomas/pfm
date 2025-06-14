module DB.User
    ( UserRow (..)
    ) where

import Data.Text
import Database.SQLite.Simple
    ( FromRow (fromRow)
    , field
    )

data UserRow = MkUserRow
    { userRowId :: Int
    , userRowName :: Text
    , userRowEmail :: Text
    , userRowCreatedAt :: Int
    , userRowUpdatedAt :: Int
    }

instance FromRow UserRow where
    fromRow = do
        MkUserRow
            <$> field
            <*> field
            <*> field
            <*> field
            <*> field
