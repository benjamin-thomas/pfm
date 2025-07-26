module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import SQLite3 (DBConnection)
import SQLite3 as SQLite3
import Server.Database (initDatabase, createSchema)
import Server.Main (startServer)
import Test.Api.Spec as ApiSpec
import Test.Database.Spec as DatabaseSpec
import Test.Database.TestUtils (setupTestFixtures)
import Test.OfxParser.Spec as OfxParserSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

foreign import removeFile :: String -> Effect Unit
foreign import getRandomPort :: Effect Int
foreign import getUniqueId :: Effect String

-- Removed seedDatabase function - now using setupTestFixtures from TestUtils

main :: Effect Unit
main = do
  log "Starting test server..."
  uniqueId <- getUniqueId
  let dbPath = "./db.integration-test-" <> uniqueId <> ".sqlite"
  log $ "Using test database: " <> dbPath
  
  initDatabase dbPath # runAff_
    case _ of
      Left err -> throw $ "Failed to initialize database: " <> show err
      Right db -> do
        -- Create full schema for integration tests
        createSchema db # runAff_
          case _ of
            Left err -> throw $ "Failed to create tables: " <> show err
            Right _ -> setupTestFixtures db # runAff_
              case _ of
                Left err -> throw $ "Failed to setup test fixtures: " <> show err
                Right _ -> do
                  liftEffect $ log "Running database tests only (server disabled temporarily)..."
                  runSpecAndExitProcess [ consoleReporter ] do
                    OfxParserSpec.spec
                    DatabaseSpec.spec db
                    -- ApiSpec.spec port -- Disabled for now

