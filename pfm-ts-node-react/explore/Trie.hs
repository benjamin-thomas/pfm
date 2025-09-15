{-# LANGUAGE OverloadedStrings #-}

{-
   HTTP Route Trie in Haskell

   This shows how naturally Tries work in Haskell with:
   - Immutable data structures by default
   - Pattern matching
   - Maybe monad for optional values
   - foldl for traversal

stack ghci --package pretty-simple --package containers ./Trie.hs
ghci> import Text.Pretty.Simple (pPrint)
ghci> emptyNode & addMethod "GET" & pPrint
ghci> pPrint buildSampleTrie

---

Or:

stack ghci --ghci-options "-interactive-print=Text.Pretty.Simple.pPrint" --package pretty-simple --package containers ./Trie.hs

-}

import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set

-- The Trie node type - algebraic data types make this clean
data TrieNode = TrieNode
  { children :: Map String TrieNode -- Map from segment to child
  , methods :: Set String -- HTTP methods at this node
  }
  deriving (Show, Eq)

-- State during traversal
data TrieState = TrieState
  { currentNode :: Maybe TrieNode -- Nothing means "not found"
  , capturedParams :: [String] -- Parameters from ? wildcards
  }
  deriving (Show, Eq)

-- Create an empty node
emptyNode :: TrieNode
emptyNode = TrieNode Map.empty Set.empty

-- Add a child to a node (returns new node - immutable!)
addChild :: String -> TrieNode -> TrieNode -> TrieNode
addChild segment child node =
  node{children = Map.insert segment child (children node)}

-- Add a method to a node
addMethod :: String -> TrieNode -> TrieNode
addMethod method node =
  node{methods = Set.insert method (methods node)}

-- The fold function - applied at each navigation step
-- This is the heart of the algorithm!
navigationStep :: TrieState -> String -> TrieState
navigationStep state segment =
  case currentNode state of
    Nothing ->
      -- Already at dead end, stay there
      state
    Just node ->
      -- First try exact match
      case Map.lookup segment (children node) of
        Just child ->
          -- Found exact match
          TrieState (Just child) (capturedParams state)
        Nothing ->
          -- No exact match, try wildcard
          case Map.lookup "?" (children node) of
            Just child ->
              -- Found wildcard, capture the parameter
              TrieState (Just child) (segment : capturedParams state)
            Nothing ->
              -- No match at all
              TrieState Nothing (capturedParams state)

-- Navigate through the trie using foldl'
navigate :: TrieNode -> [String] -> TrieState
navigate root segments =
  let initialState = TrieState (Just root) []
   in foldl navigationStep initialState segments

-- Build a sample trie
buildSampleTrie :: TrieNode
buildSampleTrie =
  let
    -- Create /users node with GET method
    usersNode =
      emptyNode
        & addMethod "GET"

    -- Create /users/? wildcard node with GET and DELETE

    -- Update users node to have wildcard child
    usersWithWildcard =
      (emptyNode & addMethod "GET") & (emptyNode & addMethod "GET" & addMethod "DELETE" & addChild "?")

    -- Build root with all paths
    root =
      emptyNode
        & addChild
          "users"
          ( -- /users node with GET method
            emptyNode
              & addMethod "GET"
              & ( -- /users/? wildcard node with GET and DELETE
                  emptyNode & addMethod "GET" & addMethod "DELETE" & addChild "?"
                )
          )
        & addChild "posts" (emptyNode & addMethod "GET") -- Create /posts node with GET method
   in
    root

-- Build the same trie using normal function application (no pipeline operators)
buildSampleTrie2 :: TrieNode
buildSampleTrie2 =
  let
    -- Create /users/? wildcard node with GET and DELETE methods
    userIdNode = addMethod "DELETE" (addMethod "GET" emptyNode)

    -- Create /users node with GET method and wildcard child
    usersNode = addChild "?" userIdNode (addMethod "GET" emptyNode)

    -- Create /posts node with GET method
    postsNode = addMethod "GET" emptyNode

    -- Build root with both children
    root = addChild "posts" postsNode (addChild "users" usersNode emptyNode)
   in
    root

buildSampleTrie3 :: TrieNode
buildSampleTrie3 =
  addChild
    "posts"
    (addMethod "GET" emptyNode)
    ( addChild
        "users"
        ( addChild
            "?"
            (addMethod "DELETE" (addMethod "GET" emptyNode))
            (addMethod "GET" emptyNode)
        )
        emptyNode
    )

{- FOURMOLU_DISABLE -}
buildSampleTrie4 :: TrieNode
buildSampleTrie4 =
  emptyNode
    & addChild "users"
        ( emptyNode
            & addMethod "GET"
            & addChild "?"
                ( emptyNode
                    & addMethod "GET"
                    & addMethod "DELETE"
                )
        )
    & addChild "posts"
        ( emptyNode
            & addMethod "GET"
        )
    & addChild "hello"
        ( emptyNode
            & addChild "?"
                ( emptyNode
                    & addMethod "GET"
                )
        )
{- FOURMOLU_ENABLE -}

-- Demonstrate fold iterations step by step
demonstrateFoldIterations :: IO ()
demonstrateFoldIterations = do
  let root = buildSampleTrie

  putStrLn "=== Haskell Trie Navigation Demo ===\n"

  -- Show iteration 0: Start at root
  let state0 = TrieState (Just root) []
  putStrLn $ "Iteration 0 (initial): node=" ++ show state0 ++ ", params=[]"

  -- Iteration 1: Navigate to "users"
  let state1 = navigationStep state0 "users"
  putStrLn $ "Iteration 1 (users):   node=" ++ show state1 ++ ", params=" ++ show (capturedParams state1)

  -- Iteration 2: Navigate to "123" (matches wildcard)
  let state2 = navigationStep state1 "123"
  putStrLn $ "Iteration 2 (123):     node=" ++ show state2 ++ ", params=" ++ show (reverse $ capturedParams state2)

  -- Check if GET method exists at final node
  case currentNode state2 of
    Just node
      | Set.member "GET" (methods node) ->
          putStrLn "✓ GET /users/123 found!"
    Just _ ->
      putStrLn "✗ No GET method at this path"
    Nothing ->
      putStrLn "✗ Path not found"

  putStrLn "\n=== Using foldl' (the natural way) ===\n"

  -- Test various paths
  let testPaths =
        [ (["users", "123"], "GET")
        , (["users"], "GET")
        , (["posts"], "GET")
        , (["posts", "456"], "GET") -- This won't match
        ]

  mapM_ (testPath root) testPaths
 where
  showNode :: TrieState -> String
  showNode state = case currentNode state of
    Just _ -> "Some(...)"
    Nothing -> "Nothing"

  testPath :: TrieNode -> ([String], String) -> IO ()
  testPath root (segments, method) = do
    let finalState = navigate root segments
        path = "/" ++ concatMap (++ "/") (init segments) ++ last segments
    case currentNode finalState of
      Just node
        | Set.member method (methods node) ->
            putStrLn $
              "✓ "
                ++ method
                ++ " "
                ++ path
                ++ " -> Found! Params: "
                ++ show (reverse $ capturedParams finalState)
      Just _ ->
        putStrLn $ "✗ " ++ method ++ " " ++ path ++ " -> Path found but no " ++ method ++ " method"
      Nothing ->
        putStrLn $ "✗ " ++ method ++ " " ++ path ++ " -> Path not found"

-- Alternative: Show the power of pattern matching with guards
matchRoute :: TrieNode -> String -> [String] -> Maybe [String]
matchRoute root method segments =
  case navigate root segments of
    TrieState (Just node) params
      | Set.member method (methods node) -> Just (reverse params)
      | otherwise -> Nothing
    _ -> Nothing

-- Main function
main :: IO ()
main = do
  demonstrateFoldIterations

  putStrLn "\n=== Pattern Matching Style ===\n"

  let root = buildSampleTrie

  -- Using pattern matching to handle results
  case matchRoute root "GET" ["users", "456"] of
    Just params -> putStrLn $ "Matched! Captured params: " ++ show params
    Nothing -> putStrLn "No match"

  case matchRoute root "DELETE" ["users", "789"] of
    Just params -> putStrLn $ "Matched! Captured params: " ++ show params
    Nothing -> putStrLn "No match"