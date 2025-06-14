module Domain.Category
    ( Category (..)
    , isStale
    , fmtCategory
    , fromCategoryRow
    ) where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import DB.Category (CategoryRow (..))

data Category = MkCategory
    { categoryId :: Int
    , categoryName :: String
    , categoryTouchedAt :: Int
    }

fromCategoryRow :: CategoryRow -> Category
fromCategoryRow
    ( MkCategoryRow
            { categoryRowId = categoryRowId'
            , categoryRowName = categoryRowName'
            , categoryRowCreatedAt = _
            , categoryRowUpdatedAt = categoryUpdatedAt'
            }
        ) =
        MkCategory
            { categoryId = categoryRowId'
            , categoryName = categoryRowName'
            , categoryTouchedAt = categoryUpdatedAt'
            }

{-

These functions below represent "business logic".

So, it makes sens to have this kind of flow:

CategoryRow => Category => CategoryDTO

---

cabal repl --repl-options "-interactive-print=Text.Pretty.Simple.pPrint" --build-depends pretty-simple

ghci> :m +Database.Category Domain.Category
ghci> categories <- getCategories =<< newConn
ghci> map fmtCategory categories
ghci> mapM isStale categories

-}

{- | Check if a category hasn't been updated in more than 90 days
This is pure domain logic that doesn't belong in DB or DTO layers
-}
isStale :: Category -> IO Bool
isStale category = do
    currentTime <- getCurrentTime
    let updatedAt = posixSecondsToUTCTime (fromIntegral $ categoryTouchedAt category)
        diffSeconds = diffUTCTime currentTime updatedAt
        ninetyDaysInSeconds = 90 * 24 * 60 * 60
    return $ diffSeconds > ninetyDaysInSeconds

{- | Format a category for display with additional context
This is domain logic that transforms the core entity for presentation
-}
fmtCategory :: Category -> String
fmtCategory category =
    mconcat
        [ "#" <> show (categoryId category)
        , ": "
        , categoryName category
        ]
