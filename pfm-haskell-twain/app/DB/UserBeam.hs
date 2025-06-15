{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module DB.UserBeam where

import Data.Int (Int32)
import Data.Text
import Database.Beam
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (Connection)

data UserT f = User
    { _userId :: Columnar f Int32
    , _userFirstName :: Columnar f Text
    , _userLastName :: Columnar f Text
    , _userEmail :: Columnar f Text
    , _userCreatedAt :: Columnar f Int32
    , _userUpdatedAt :: Columnar f Int32
    }
    deriving (Generic, Beamable)

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int32) deriving (Generic, Beamable)
    primaryKey = UserId . _userId

-- Helper types
type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show (PrimaryKey UserT Identity)
deriving instance Show User

newtype MyDb f = MyDb
    { _myDbUsers :: f (TableEntity UserT)
    }
    deriving (Generic, Database be)

myDb :: DatabaseSettings be MyDb
myDb =
    defaultDbSettings
        `withDbModification` dbModification
            { _myDbUsers =
                mconcat
                    [ setEntityName "users"
                    , modifyTableFields
                        tableModification
                            { _userId = "user_id"
                            , _userFirstName = "first_name"
                            }
                    ]
            }

{-

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m + DB.UserBeam
ghci> newConn >>= getAllUsers

 -}
getAllUsers :: Connection -> IO [User]
getAllUsers conn =
    runBeamSqliteDebug putStrLn conn
        . runSelectReturningList
        . select
        $ do
            user <- all_ (_myDbUsers myDb)
            guard_ (_userEmail user /=. "bogus@example.com")
            return user
