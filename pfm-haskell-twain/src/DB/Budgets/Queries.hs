{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Budgets.Queries where

import Data.FileEmbed (embedFile)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query (Query), ToRow, query)
import GHC.Generics

data BudgetNewRow = MkBudgetNewRow
    { startsOn :: Int
    , endsOn :: Int
    }
    deriving (Show, Generic, ToRow)

toUnix :: Day -> Int
toUnix d =
    truncate . utcTimeToPOSIXSeconds $ UTCTime d 0

{-
cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple pfm-haskell-twain-lib
ghci> :m + DB.Budgets.Queries Data.Time.Calendar
ghci> _newConn >>= flip getBudgetIdForDateTry (fromGregorian 2025 7 15)
 -}
getBudgetIdForDateTry :: Connection -> Day -> IO (Maybe Int)
getBudgetIdForDateTry conn day =
    fmap
        -- (\rows -> fmap fromOnly (listToMaybe rows))
        (fmap fromOnly . listToMaybe)
        ( query
            conn
            (Query $ decodeUtf8 $(embedFile "src/DB/Budgets/getBudgetIdForDate.sql"))
            (Only $ toUnix day)
        )

{-
ghci> :m + DB.Budgets.Queries Data.Time.Calendar
ghci> _newConn >>= flip insertForDateExn (fromGregorian 2025 7 15)
 -}
insertForDateExn :: Connection -> Day -> IO Int
insertForDateExn conn day =
    fmap
        (fromOnly . head)
        ( query
            conn
            (Query $ decodeUtf8 $(embedFile "src/DB/Budgets/insertForDate.sql"))
            (Only $ toUnix day)
        )
