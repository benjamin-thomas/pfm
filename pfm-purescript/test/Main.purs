module Test.Main where

import Prelude

import Affjax.Node as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array (length)
import Data.Either (Either(..))
import Effect (Effect)
import Shared.Types (User)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Yoga.JSON as JSON

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "PFM API" do
    it "should return server status" do
      result <- AX.get ResponseFormat.string "http://localhost:8080/"
      case result of
        Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
        Right response -> shouldEqual (StatusCode 200) response.status

    it "should return users list" do
      result <- AX.get ResponseFormat.string "http://localhost:8080/users"
      case result of
        Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
        Right response -> do
          shouldEqual (StatusCode 200) response.status
          case JSON.readJSON response.body of
            Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
            Right (users :: Array User) ->
              shouldEqual true (length users >= 0) -- Should have at least 0 users