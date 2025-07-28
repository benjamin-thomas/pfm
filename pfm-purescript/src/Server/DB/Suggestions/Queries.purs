module Server.DB.Suggestions.Queries
  ( SuggestedAccountDB
  , SuggestionInsert
  , SuggestionRowDB(..)
  , batchApplySuggestions
  , convertRowToSuggestion
  , getSuggestions
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Effect.Aff (Aff, throwError, attempt)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Server.DB.Transactions.Queries as TransactionQueries
import Server.DB.Utils (fromDbRows)
import SQLite3 (DBConnection, queryDB)
import Shared.Types (Suggestion(..), SuggestedAccount(..), Transaction(..))
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

-- | Represents a suggestion update from the frontend
type SuggestionInsert =
  { transactionId :: Int
  , toAccountId :: Int
  }

-- | Apply multiple suggestion updates in a batch
-- | Takes an array of transaction ID and target account ID pairs
batchApplySuggestions :: Array SuggestionInsert -> DBConnection -> Aff Unit
batchApplySuggestions suggestions db = do
  liftEffect $ log $ "Applying " <> show (Array.length suggestions) <> " suggestions in batch"

  -- Try to start a transaction, if we're already in one, use a savepoint
  transactionResult <- attempt $ queryDB db "BEGIN TRANSACTION" []

  case transactionResult of
    Left _ -> do
      -- We're already in a transaction, use a savepoint
      liftEffect $ log "Already in transaction, using savepoint for batch operation"
      _ <- queryDB db "SAVEPOINT batch_suggestions_sp" []

      -- Create temporary table and perform updates
      performBatchUpdate

      -- Release the savepoint (commits the changes within the transaction)
      _ <- queryDB db "RELEASE batch_suggestions_sp" []
      pure unit

    Right _ -> do
      -- We started a new transaction
      liftEffect $ log "Started new transaction for batch operation"

      -- Create temporary table and perform updates
      performBatchUpdate

      -- Commit the transaction
      _ <- queryDB db "COMMIT" []
      pure unit

  where
  performBatchUpdate :: Aff Unit
  performBatchUpdate = do
    -- Create temporary table
    _ <- queryDB db "CREATE TEMPORARY TABLE tmp (transaction_id INTEGER, to_account_id INTEGER)" []

    -- Insert all suggestions into temp table
    traverse_ insertSuggestion suggestions

    -- Perform batch update
    _ <- queryDB db updateSql []

    -- Clean up temp table
    _ <- queryDB db "DROP TABLE tmp" []

    liftEffect $ log "Batch suggestion update completed"

  insertSuggestion :: SuggestionInsert -> Aff Unit
  insertSuggestion suggestion = do
    _ <- queryDB db
      "INSERT INTO tmp (transaction_id, to_account_id) VALUES (?, ?)"
      [ JSON.writeImpl suggestion.transactionId
      , JSON.writeImpl suggestion.toAccountId
      ]
    pure unit

  updateSql =
    """
      UPDATE transactions AS t
      SET to_account_id = tmp.to_account_id
      FROM tmp
      WHERE t.transaction_id = tmp.transaction_id
    """