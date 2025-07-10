{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module DB.Budgets.Queries where

import Data.Aeson
import Data.FileEmbed (embedFile)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.SQLite.Simple
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
ghci> :m + DB.Budgets.Queries Data.Time.Calendar
ghci> _newConn >>= flip getBudgetIdForDateTry (fromGregorian 2025 7 15)
 -}
getBudgetIdForDateTry :: Connection -> Day -> IO (Maybe Int)
getBudgetIdForDateTry conn day =
    fmap
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

data BudgetDB = MkBudgetDB
    { budgetId :: Int
    , budgetStartsOn :: UTCTime
    , budgetEndsOn :: UTCTime
    , budgetCreatedAt :: UTCTime
    , budgetUpdatedAt :: UTCTime
    }
    deriving (Show, Generic, FromRow)

{-
ghci> :m + DB.Budgets.Queries
ghci> _newConn >>= getAll
 -}
getAll :: Connection -> IO [BudgetDB]
getAll conn =
    query_
        conn
        (Query $ decodeUtf8 $(embedFile "src/DB/Budgets/getAll.sql"))

data BudgetWithLinesDB = MkBudgetWithLinesDB
    { budgetId :: Int
    , startsOn :: UTCTime
    , endsOn :: UTCTime
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , lines :: [BudgetLineDB]
    }
    deriving (Show, Generic)

data BudgetLineDB = MkBudgetLineDB
    { budgetLineId :: Int
    , budgetId :: Int
    , accountId :: Int
    , cents :: Int
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    }
    deriving (Show, Generic)

instance FromJSON BudgetLineDB where
    parseJSON = withObject "BudgetLineDB" $ \v ->
        MkBudgetLineDB
            <$> v .: "budgetLineId"
            <*> v .: "budgetId"
            <*> v .: "accountId"
            <*> v .: "cents"
            <*> v .: "createdAtUtc"
            <*> v .: "updatedAtUtc"

instance FromRow BudgetWithLinesDB where
    fromRow =
        let
            decodeLines :: Text -> [BudgetLineDB]
            decodeLines linesJSON =
                either
                    (error . show)
                    id
                    (eitherDecodeStrictText linesJSON)
         in
            MkBudgetWithLinesDB
                <$> field
                <*> field
                <*> field
                <*> field
                <*> field
                <*> (decodeLines <$> field)

{-
ghci> :m + DB.Budgets.Queries
ghci> _newConn >>= getOne 1
 -}
getOne :: Int -> Connection -> IO BudgetWithLinesDB
getOne budgetId' conn =
    fmap
        head
        ( query
            conn
            (Query $ decodeUtf8 $(embedFile "src/DB/Budgets/getOne.sql"))
            (Only budgetId')
        )
