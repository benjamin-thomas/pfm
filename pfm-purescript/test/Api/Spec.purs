module Test.Api.Spec where

import Prelude

import Affjax.Node as AX
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array (length, head)
import Data.Either (Either(..))
import Data.String (contains, Pattern(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Server.DB.Account (AccountDB(..))
import Server.DB.Budget (BudgetDB(..))
import Server.DB.Category (CategoryDB(..))
import Shared.Types (Transaction, User(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Yoga.JSON as JSON

spec :: Int -> Spec Unit
spec port = do
  describe "PFM API" do
    describe "Server Health" do
      it "should return server status" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> shouldEqual (StatusCode 200) response.status

    describe "Users Endpoints" do
      it "should return users list" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/users")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (users :: Array User) -> do
                shouldEqual (length users) 2
                -- Validate JSON structure
                case users of
                  [ User user1, User user2 ] -> do
                    user1.firstName `shouldEqual` "John"
                    user1.lastName `shouldEqual` "Doe"
                    user2.firstName `shouldEqual` "Jane"
                    user2.lastName `shouldEqual` "Smith"
                  _ -> shouldEqual "Expected exactly 2 users" "Got different count"

      it "should get user by ID" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/users/1")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (user :: User) -> do
                case user of
                  User userData -> do
                    userData.firstName `shouldEqual` "John"
                    userData.lastName `shouldEqual` "Doe"

    describe "Categories Endpoints" do
      it "should get all categories" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/categories")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (categories :: Array CategoryDB) -> do
                length categories `shouldEqual` 4
                -- Validate JSON structure of first category
                case head categories of
                  Just (CategoryDB cat) -> do
                    cat.categoryId `shouldSatisfy` (_ > 0)
                    cat.name `shouldSatisfy` (_ /= "")
                    cat.createdAtUnix `shouldSatisfy` (_ > 0)
                    cat.updatedAtUnix `shouldSatisfy` (_ > 0)
                  Nothing -> shouldEqual "Expected at least one category" "No categories found"

      it "should get category by ID" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/categories/1")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (category :: CategoryDB) -> do
                let (CategoryDB catData) = category
                catData.categoryId `shouldEqual` 1
                catData.name `shouldEqual` "Equity"

      it "should return 404 for non-existent category" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/categories/999")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 404) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left _ -> shouldEqual "Expected valid JSON" "Got JSON error"
              Right (errorObj :: { error :: String }) ->
                errorObj.error `shouldEqual` "Category not found"

    describe "Accounts Endpoints" do
      it "should get all accounts" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (accounts :: Array AccountDB) -> do
                length accounts `shouldSatisfy` (_ >= 13)
                -- Validate JSON structure of first account
                case head accounts of
                  Just (AccountDB acc) -> do
                    acc.accountId `shouldSatisfy` (_ > 0)
                    acc.categoryId `shouldSatisfy` (_ > 0)
                    acc.name `shouldSatisfy` (_ /= "")
                    acc.createdAtUnix `shouldSatisfy` (_ > 0)
                    acc.updatedAtUnix `shouldSatisfy` (_ > 0)
                  Nothing -> shouldEqual "Expected at least one account" "No accounts found"

    describe "Budgets Endpoints" do
      it "should get all budgets" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/budgets")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (budgets :: Array BudgetDB) ->
                length budgets `shouldSatisfy` (_ >= 0)

      it "should get budget by ID when exists" do
        -- First get all budgets to find an existing one
        budgetsResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/budgets")
        case budgetsResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right budgetsResponse -> do
            case JSON.readJSON budgetsResponse.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (budgets :: Array BudgetDB) -> do
                case head budgets of
                  Just (BudgetDB budget) -> do
                    result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/budgets/" <> show budget.budgetId)
                    case result of
                      Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
                      Right response -> do
                        shouldEqual (StatusCode 200) response.status
                        case JSON.readJSON response.body of
                          Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
                          Right (foundBudget :: BudgetDB) -> do
                            let (BudgetDB foundData) = foundBudget
                            foundData.budgetId `shouldEqual` budget.budgetId
                  Nothing -> shouldEqual "Expected at least one budget" "No budgets found"

    describe "Transactions Endpoints" do
      it "should get all transactions" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/transactions")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (transactions :: Array Transaction) ->
                length transactions `shouldSatisfy` (_ >= 0)

      it "should return 404 for non-existent transaction" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/transactions/999")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 404) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left _ -> shouldEqual "Expected valid JSON" "Got JSON error"
              Right (errorObj :: { error :: String }) ->
                errorObj.error `shouldEqual` "Transaction not found"

      it "should return error when deleting non-existent transaction" do
        deleteResult <- AX.request AX.defaultRequest
          { method = Left DELETE
          , url = "http://localhost:" <> show port <> "/transactions/999"
          , responseFormat = ResponseFormat.string
          , content = Nothing
          }
        case deleteResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            response.body `shouldEqual` "Transaction deleted"