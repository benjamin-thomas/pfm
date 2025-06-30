{-# LANGUAGE OverloadedStrings #-}

module Similarity where

import Data.Char (isAlphaNum, toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.Set qualified as S
import System.IO

-- | Normalize a string by removing non-alphanum chars and lowercasing
normalize :: String -> String
normalize = map toLower . filter (\c -> isAlphaNum c || c == ' ')

-- | Tokenize a string into a set of lowercase words
tokenize :: String -> S.Set String
tokenize = S.fromList . words . normalize

-- | Jaccard similarity between two token sets
jaccard :: S.Set String -> S.Set String -> Double
jaccard a b
    | S.null union = 0
    | otherwise = fromIntegral (S.size inter) / fromIntegral (S.size union)
  where
    inter = S.intersection a b
    union = S.union a b

-- | Naive similarity: number of common words
naiveOverlap :: S.Set String -> S.Set String -> Int
naiveOverlap a b = S.size $ S.intersection a b

-- | Levenshtein distance (edit distance)
levenshtein :: String -> String -> Int
levenshtein s t = last (foldl transform [0 .. length s] t)
  where
    transform xs@(x : xs') c = scanl compute (x + 1) (zip3 s xs xs')
      where
        compute z (c', x', y') = minimum [y' + 1, z + 1, x' + fromEnum (c' /= c)]

-- | Sample transactions (raw descriptions)
transactions :: [String]
transactions =
    [ "ORANGE SA-ORANGE"
    , "ORANGE MOBILE BILL"
    , "NETFLIX 12/06"
    , "SPAR PARIS 10/05"
    , "SPOTIFY FAMILY PLAN"
    , "AMAZON FRANCE SAS"
    ]

main :: IO ()
main = do
    putStrLn "Enter a transaction description to compare:"
    hFlush stdout
    input <- getLine

    let inputTok = tokenize input

    putStrLn "\n--- Similarity Rankings ---\n"

    let scored =
            map
                ( \s ->
                    ( s
                    , jaccard inputTok (tokenize s)
                    , naiveOverlap inputTok (tokenize s)
                    , levenshtein input s
                    )
                )
                transactions

    putStrLn "By Jaccard similarity:"
    mapM_ print $ take 3 $ reverse $ sortOn (\(_, score, _, _) -> score) scored

    putStrLn "\nBy naive word overlap:"
    mapM_ print $ take 3 $ reverse $ sortOn (\(_, _, overlap, _) -> overlap) scored

    putStrLn "\nBy Levenshtein distance (lower is better):"
    mapM_ print $ take 3 $ sortOn (\(_, _, _, dist) -> dist) scored

-- Helper: sort list using a function
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = map snd . sortPairs . map (\x -> (f x, x))
  where
    sortPairs = sortBy (compare `on` fst)
