module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Server.Database (initDatabase, createSchema)
import Server.Main (startServer, AppEnv(..))
import Test.Api.Spec as ApiSpec
import Test.Database.Spec as DatabaseSpec
import Test.Database.TestUtils (setupTestFixtures)
import Test.OfxParser.Spec as OfxParserSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

foreign import removeFile :: String -> Effect Unit
foreign import getRandomPort :: Effect Int
foreign import getUniqueId :: Effect String
foreign import registerExitHandler :: Effect Unit -> Effect Unit

-- Removed seedDatabase function - now using setupTestFixtures from TestUtils

main :: Effect Unit
main = do
  log "Starting test server..."
  uniqueId <- getUniqueId
  let dbPath = "./db.integration-test-" <> uniqueId <> ".sqlite"
  log $ "Using test database: " <> dbPath

  -- Register cleanup handler to remove test database on exit
  registerExitHandler do
    log $ "Cleaning up test database: " <> dbPath
    removeFile dbPath

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
                  port <- liftEffect getRandomPort
                  liftEffect $ log $ "Starting test server on port: " <> show port
                  liftEffect $ void $ startServer port TestEnv db
                  liftEffect $ log "Test server started successfully"
                  let testConfig = defaultConfig -- { failFast = true }

                  {-


                  spago test --exec-args --help
                  spago test --exec-args --only-failures
                  spago test --exec-args --fail-fast

                  # Or combines both
                  spago test --exec-args --next-failure


                   -}
                  liftEffect $ runSpecAndExitProcess'
                    { defaultConfig: testConfig
                    , parseCLIOptions: true -- Allow CLI to override failFast
                    }
                    [ consoleReporter ]
                    do
                      OfxParserSpec.spec
                      DatabaseSpec.spec db
                      ApiSpec.spec port

