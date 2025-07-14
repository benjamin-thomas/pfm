module Test.Api.Spec where

import Prelude

import Affjax.Node as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array (length)
import Data.Either (Either(..))
import Shared.Types (User)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.JSON as JSON

spec :: Int -> Spec Unit
spec port = do
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
              shouldEqual (length users) 2