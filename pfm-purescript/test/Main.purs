module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (throw)
import Server.Database (initDatabase, seedDatabase, createSchema)
import Server.Main (startServer)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.OfxParser.Spec as OfxParserSpec
import Test.Api.Spec as ApiSpec
import Test.Database.Spec as DatabaseSpec

foreign import removeFile :: String -> Effect Unit
foreign import getRandomPort :: Effect Int

main :: Effect Unit
main = do
  log "Starting test server..."
  removeFile "./db.test.sqlite"
  initDatabase "./db.test.sqlite" # runAff_
    case _ of
      Left err -> throw $ "Failed to initialize database: " <> show err
      Right db -> do
        -- Create full schema for integration tests
        createSchema db # runAff_
          case _ of
            Left err -> throw $ "Failed to create tables: " <> show err
            Right _ -> seedDatabase db # runAff_
              case _ of
                Left err -> throw $ "Failed to seed database: " <> show err
                Right _ -> do
                  port <- getRandomPort
                  void $ startServer port db
                  log $ "Running tests on port " <> show port <> "..."
                  runSpecAndExitProcess [ consoleReporter ] do
                    OfxParserSpec.spec
                    DatabaseSpec.spec db
                    ApiSpec.spec port

