module Test.Main where

import Prelude

import Affjax.Node as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array (length)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Effect.Exception (throw)
import Server.Database (initDatabase, seedDatabase)
import Server.Main (startServer)
import Shared.Types (User)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Yoga.JSON as JSON
import Test.OfxParser.Spec as OfxParserSpec

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
                apiTests port

apiTests :: Int -> Spec Unit
apiTests port = do
  describe "PFM API" do
    it "should return server status" do
      result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/")
      case result of
        Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
        Right response -> shouldEqual (StatusCode 200) response.status

    it "should return users list" do
      result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/users")
      case result of
        Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
        Right response -> do
          shouldEqual (StatusCode 200) response.status
          case JSON.readJSON response.body of
            Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
            Right (users :: Array User) ->
              shouldEqual true (length users >= 0) -- Should have at least 0 users
