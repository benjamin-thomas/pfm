module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (throw)
import Server.Database (initDatabase, seedDatabase)
import Server.Main (startServer)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.OfxParser.Spec as OfxParserSpec
import Test.Api.Spec as ApiSpec

foreign import removeFile :: String -> Effect Unit

main :: Effect Unit
main = do
  log "Starting test server..."
  removeFile "./db.test.sqlite"
  initDatabase "./db.test.sqlite" # runAff_
    case _ of
      Left err -> throw $ "Failed to initialize database: " <> show err
      Right db -> do
        seedDatabase db # runAff_
          case _ of
            Left err -> throw $ "Failed to seed database: " <> show err
            Right _ -> do
              let port = 7777
              void $ startServer port db
              log "Running tests..."
              runSpecAndExitProcess [ consoleReporter ] do
                OfxParserSpec.spec
                ApiSpec.spec port

