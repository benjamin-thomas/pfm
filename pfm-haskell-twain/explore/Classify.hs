{-# LANGUAGE OverloadedStrings #-}

module Classify where

import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- 1. Training data
trainingData :: [(Text, Text)]
trainingData =
    [ ("PAYPAL CRUNCHYROLL", "Entertainment")
    , ("SPAR NICE", "Groceries")
    , ("AMAZON FR", "Shopping")
    , ("ELECTRICITE DE FRANCE", "Utilities")
    ]

-- 2. Tokenize
tokenize :: Text -> Set Text
tokenize = Set.fromList . T.words . T.toUpper

-- 3. Score similarity
similarity :: Text -> Text -> Int
similarity input sample =
    Set.size $ Set.intersection (tokenize input) (tokenize sample)

-- 4. Classify new text
classify :: Text -> Text
classify input =
    snd $ maximumBy (comparing (similarity input . fst)) trainingData

-- Try it
main :: IO ()
main = do
    print $ classify "SPAR PARIS 12/06" -- "Groceries"
    print $ classify "NETFLIX 05/06" -- "Entertainment"
    print $ classify "ELECTRICITE DE MARSEILLE" -- "Utilities"
