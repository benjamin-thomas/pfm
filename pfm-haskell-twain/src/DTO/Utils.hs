module DTO.Utils (dropAndLowerHead) where

import Data.Char qualified as Char

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
