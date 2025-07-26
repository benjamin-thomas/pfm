module Test.Database.TestUtils
  ( withTestTransaction
  , setupTestFixtures
  ) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Console (log)
import SQLite3 as SQLite3

-- | Execute a test within a database transaction that will be rolled back
-- | This ensures test isolation while maintaining database state for debugging
-- | Uses SQLite savepoints to handle nested transactions properly
withTestTransaction :: forall a. SQLite3.DBConnection -> Aff a -> Aff a
withTestTransaction db testAction = do
  liftEffect $ log "Beginning test transaction..."

  -- Try to create a savepoint first - this works if we're already in a transaction
  savepointResult <- attempt $ SQLite3.queryDB db "SAVEPOINT test_isolation_sp" []

  case savepointResult of
    Right _ -> do
      -- Already in a transaction, use savepoint rollback for nested isolation
      liftEffect $ log "Using savepoint for nested transaction isolation..."
      result <- testAction
      liftEffect $ log "Rolling back to savepoint..."
      _ <- SQLite3.queryDB db "ROLLBACK TO test_isolation_sp" []
      _ <- SQLite3.queryDB db "RELEASE test_isolation_sp" [] -- Clean up savepoint
      pure result
    Left _ -> do
      -- No transaction exists, start one with BEGIN then use savepoint
      liftEffect $ log "Starting new transaction with savepoint..."
      _ <- SQLite3.queryDB db "BEGIN" []
      _ <- SQLite3.queryDB db "SAVEPOINT test_isolation_sp" []
      result <- testAction
      liftEffect $ log "Rolling back entire transaction..."
      _ <- SQLite3.queryDB db "ROLLBACK" [] -- This rolls back everything including savepoint
      pure result

-- | Set up test fixtures by creating basic test data directly
-- | This should be called once at the start of the test suite
setupTestFixtures :: SQLite3.DBConnection -> Aff Unit
setupTestFixtures _db = do
  liftEffect $ log "Setting up test fixtures..."
  -- For now, we'll rely on the schema creation in createSchema
  -- which already inserts the basic categories and accounts
  -- In the future, we can add more specific test data here if needed
  liftEffect $ log "Test fixtures setup complete (using schema defaults)"