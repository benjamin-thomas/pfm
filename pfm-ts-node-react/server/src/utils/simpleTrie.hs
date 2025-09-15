-- A minimal Trie in Haskell with insert, search, and ASCII visualization.
-- Run:  runghc src/utils/simpleTrie.hs

module Main where

import Data.List (foldl')
import Data.Map.Strict qualified as M

-- Trie node: whether this node is the end of a word and its children by character
data Trie = Trie
  { end :: !Bool
  , children :: !(M.Map Char Trie)
  }
  deriving (Eq, Show)

-- Empty trie
empty :: Trie
empty = Trie False M.empty

-- Insert a word into the trie
insert :: String -> Trie -> Trie
insert [] (Trie _ ch) = Trie True ch
insert (c : cs) (Trie e ch) =
  let child = M.findWithDefault empty c ch
      child' = insert cs child
   in Trie e (M.insert c child' ch)

-- Search for a word in the trie
search :: String -> Trie -> Bool
search [] (Trie e _) = e
search (c : cs) (Trie _ ch) =
  case M.lookup c ch of
    Nothing -> False
    Just tr' -> search cs tr'

-- Pretty ASCII visualization (similar to a console tree)
pretty :: Trie -> String
pretty t =
  unlines $ ("(root)" ++ rootMark) : go "" (M.toAscList (children t))
 where
  rootMark = if end t then "•" else ""

  markLast :: [a] -> [(Bool, a)]
  markLast [] = []
  markLast [x] = [(True, x)]
  markLast (x : xs) = (False, x) : markLast xs

  go :: String -> [(Char, Trie)] -> [String]
  go _ [] = []
  go pref entries =
    concatMap render (markLast entries)
   where
    render (isLast, (ch, child)) =
      let connector = if isLast then "└─ " else "├─ "
          endMark = if end child then "•" else ""
          thisLine = pref ++ connector ++ [ch] ++ endMark
          nextPrefix = pref ++ (if isLast then "   " else "│  ")
          kids = M.toAscList (children child)
       in thisLine : go nextPrefix kids

-- Demo main: build a trie, print visualization, and search a few words
main :: IO ()
main = do
  let wordsToInsert = ["apple", "application", "app", "apt", "bat", "batch"]
      trie = foldl' (flip insert) empty wordsToInsert

  putStrLn "ASCII visualization:"
  putStrLn (pretty trie)

  putStrLn ""
  putStrLn $ "search \"apple\": " ++ show (search "apple" trie)
  putStrLn $ "search \"app\":   " ++ show (search "app" trie)
  putStrLn $ "search \"bad\":   " ++ show (search "bad" trie)
