module DTO.Utils (dropAndLowerHead, fromUTC) where

import Data.Char qualified as Char
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

{-

>>> dropAndLowerHead 3 "llsHelloWorld"
"helloWorld"

>>> dropAndLowerHead 1 "?"
"?"

 -}
dropAndLowerHead :: Int -> String -> String
dropAndLowerHead n str = case drop n str of
    c : cs -> Char.toLower c : cs
    _ -> str

fromUTC :: UTCTime -> Int
fromUTC = truncate . utcTimeToPOSIXSeconds