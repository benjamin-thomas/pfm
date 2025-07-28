module Server.DB.Suggestions.Queries
  ( SuggestedAccountDB
  , SuggestionRowDB(..)
  , convertRowToSuggestion
  , getSuggestions
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Traversable (traverse)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Utils (fromDbRows)
import SQLite3 (DBConnection, queryDB)
import Shared.Types (Suggestion(..), SuggestedAccount(..))
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

-- | Database row type matching SQL column names
newtype SuggestionRowDB = MkSuggestionRowDB
  { soundex_descr :: String
  , suggested_accounts :: String -- JSON string
  }

derive instance Generic SuggestionRowDB _
derive newtype instance ReadForeign SuggestionRowDB

type SuggestedAccountDB =
  { id :: Int
  , name :: String
  , occurrences :: Int
  }

-- | Get suggestions for transactions that go to unknown expense accounts
-- | Based on soundex matching with existing categorized transactions
getSuggestions :: Int -> Int -> DBConnection -> Aff (Array Suggestion)
getSuggestions fromAccountId toAccountId db = do
  sql <- FS.readTextFile UTF8 "src/Server/DB/Transactions/suggestions.sql"
  rows <- queryDB db sql [ JSON.writeImpl fromAccountId, JSON.writeImpl toAccountId ]
  suggestionRows <- fromDbRows "suggestions" identity rows -- TODO: I'm not sure about that, maybe we can simplify
  traverse convertRowToSuggestion suggestionRows

-- | Convert a database row to a Suggestion
convertRowToSuggestion :: SuggestionRowDB -> Aff Suggestion
convertRowToSuggestion (MkSuggestionRowDB row) = do
  case JSON.readJSON row.suggested_accounts of
    Left err -> throwError $ error $ "Failed to parse suggested accounts JSON: " <> show err
    Right (accounts :: Array SuggestedAccountDB) -> do
      let suggestedAccounts = map (\acc -> SuggestedAccount { accountId: acc.id, accountName: acc.name }) accounts
      pure $ Suggestion { soundexDescr: row.soundex_descr, suggestedAccounts }