module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import SQLite3 (DBConnection)
import Server.Database (initDatabase, createSchema)
import Server.Main (startServer)
import Test.Api.Spec as ApiSpec
import Test.Database.Spec as DatabaseSpec
import Test.OfxParser.Spec as OfxParserSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

foreign import removeFile :: String -> Effect Unit
foreign import getRandomPort :: Effect Int

seedDatabase :: DBConnection -> Aff Unit
seedDatabase _db = do
  log "NOOP: seedDatabase" -- temporarily turned off

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

