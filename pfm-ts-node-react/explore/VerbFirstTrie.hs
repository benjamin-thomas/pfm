{-# LANGUAGE OverloadedStrings #-}

{-
   HTTP Route Trie - VERB FIRST approach

   Instead of: Root → path → path → {methods}
   We do:      Root → method → path → path → {found}

   This eliminates the "double root" issue you noticed!

---

stack ghci --package pretty-simple --package containers ./VerbFirstTrie.hs
ghci> import Text.Pretty.Simple (pPrint)
ghci> emptyNode & addMethod "GET" & pPrint
ghci> pPrint buildSampleTrie

---

Or:

stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint" --package pretty-simple --package containers ./VerbFirstTrie.hs
-}

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- Simplified: just a boolean at leaf nodes
data TrieNode = TrieNode
  { children :: Map String TrieNode
  , isEndpoint :: Bool -- Much simpler: just "is this a valid endpoint?"
  }
  deriving (Show, Eq)

-- State during traversal - same as before
data TrieState = TrieState
  { currentNode :: Maybe TrieNode
  , capturedParams :: [String]
  }
  deriving (Show, Eq)

-- Create an empty node
emptyNode :: TrieNode
emptyNode = TrieNode Map.empty False

-- Mark a node as an endpoint
markEndpoint :: TrieNode -> TrieNode
markEndpoint node = node{isEndpoint = True}

-- Add a child to a node
addChild :: String -> TrieNode -> TrieNode -> TrieNode
addChild segment child node =
  node{children = Map.insert segment child (children node)}

-- The fold function - same navigation logic
navigationStep :: TrieState -> String -> TrieState
navigationStep state segment =
  case currentNode state of
    Nothing -> state -- Already at dead end
    Just node ->
      case Map.lookup segment (children node) of
        Just child ->
          -- Found exact match
          TrieState (Just child) (capturedParams state)
        Nothing ->
          -- Try wildcard
          case Map.lookup "?" (children node) of
            Just child ->
              -- Found wildcard, capture parameter
              TrieState (Just child) (segment : capturedParams state)
            Nothing ->
              -- No match
              TrieState Nothing (capturedParams state)

-- Navigate through segments (after method)
navigate :: TrieNode -> [String] -> TrieState
navigate root segments =
  let initialState = TrieState (Just root) []
   in foldl navigationStep initialState segments

-- Build verb-first trie
buildVerbFirstTrie :: TrieNode
buildVerbFirstTrie =
  let
    -- GET /users endpoint
    getUsersEndpoint = markEndpoint emptyNode

    -- GET /users/? endpoint
    getUserIdEndpoint = markEndpoint emptyNode
    getUsersWithId = emptyNode & addChild "?" getUserIdEndpoint
    getWithUsers =
      emptyNode
        & addChild "users" (emptyNode & addChild "?" getUserIdEndpoint)
        & addChild "users" getUsersEndpoint -- This overwrites! Problem!

    -- Let me fix this properly...
    usersNode =
      emptyNode
        & addChild "?" getUserIdEndpoint -- /users/? for GET
    usersNodeWithEndpoint = markEndpoint usersNode -- Also allow /users for GET
    getRoot =
      emptyNode
        & addChild "users" usersNodeWithEndpoint
        & addChild "posts" (markEndpoint emptyNode)

    -- DELETE /users/? endpoint
    deleteUsersId = emptyNode & addChild "?" (markEndpoint emptyNode)
    deleteRoot = emptyNode & addChild "users" deleteUsersId

    -- Root with methods as first level
    root =
      emptyNode
        & addChild "GET" getRoot
        & addChild "DELETE" deleteRoot
   in
    root

-- Lookup function - method first, then path
lookupVerbFirst :: String -> [String] -> TrieNode -> Maybe [String]
lookupVerbFirst method pathSegments root =
  case Map.lookup method (children root) of
    Nothing -> Nothing -- Method not supported
    Just methodNode ->
      let finalState = navigate methodNode pathSegments
       in case currentNode finalState of
            Just node
              | isEndpoint node ->
                  Just (reverse $ capturedParams finalState)
            _ -> Nothing

-- Test it
main :: IO ()
main = do
  let root = buildVerbFirstTrie

  putStrLn "=== Verb-First Trie Demo ===\n"

  let tests =
        [ ("GET", ["users", "123"])
        , ("GET", ["users"])
        , ("GET", ["posts"])
        , ("DELETE", ["users", "456"])
        , ("DELETE", ["posts"]) -- Should fail
        , ("POST", ["users"]) -- Should fail
        ]

  mapM_
    ( \(method, path) -> do
        case lookupVerbFirst method path root of
          Just params -> putStrLn $ "✓ " ++ method ++ " /" ++ unwords path ++ " -> Params: " ++ show params
          Nothing -> putStrLn $ "✗ " ++ method ++ " /" ++ unwords path ++ " -> Not found"
    )
    tests