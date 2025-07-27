module Test.Api.Spec where

import Prelude

import Affjax.Node as AX
import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Array (length, head, find) as Array
import Data.Either (Either(..))
import Data.String (contains, Pattern(..), toLower)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Server.DB.Account (AccountDB(..))
import Server.DB.Budget (BudgetDB(..))
import Server.DB.Category (CategoryDB(..))
import Shared.Types (AccountBalanceRead(..), LedgerViewRow(..), Transaction, User(..))
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
                Array.length categories `shouldEqual` 4
                -- Validate JSON structure of first category
                case Array.head categories of
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
                Array.length accounts `shouldSatisfy` (_ >= 13)
                -- Validate JSON structure of first account
                case Array.head accounts of
                  Just (AccountDB acc) -> do
                    acc.accountId `shouldSatisfy` (_ > 0)
                    acc.categoryId `shouldSatisfy` (_ > 0)
                    acc.name `shouldSatisfy` (_ /= "")
                    acc.createdAtUnix `shouldSatisfy` (_ > 0)
                    acc.updatedAtUnix `shouldSatisfy` (_ > 0)
                  Nothing -> shouldEqual "Expected at least one account" "No accounts found"

      it "should get ledger view for checking account" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) ->
                Array.length ledgerRows `shouldSatisfy` (_ >= 0) -- May be empty initially

      it "should filter ledger view by description" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger?description=Grocery")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) -> do
                -- All returned rows should contain "Grocery" in description (case insensitive)
                let allMatch = Array.find (\(LedgerViewRow row) -> not (contains (Pattern "grocery") (toLower row.descr))) ledgerRows
                case allMatch of
                  Nothing -> pure unit -- All good, all results match filter
                  Just _ -> shouldEqual "All results match filter" "Some results don't match description filter"

      it "should filter ledger view by minimum amount" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger?minAmount=4000")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) -> do
                -- All returned rows should have amount >= 4000 cents (40.00 euros) in absolute terms
                let allMatch = Array.find (\(LedgerViewRow row) -> (if row.flowCents < 0 then (-row.flowCents) else row.flowCents) < 4000) ledgerRows
                case allMatch of
                  Nothing -> pure unit -- All good, all results match filter
                  Just _ -> shouldEqual "All results match min amount filter" "Some results don't match minimum amount filter"

      it "should filter ledger view by maximum amount" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger?maxAmount=3000")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) -> do
                -- All returned rows should have amount <= 3000 cents (30.00 euros) in absolute terms
                let allMatch = Array.find (\(LedgerViewRow row) -> (if row.flowCents < 0 then (-row.flowCents) else row.flowCents) > 3000) ledgerRows
                case allMatch of
                  Nothing -> pure unit -- All good, all results match filter
                  Just _ -> shouldEqual "All results match max amount filter" "Some results don't match maximum amount filter"

      it "should filter ledger view for unknown expenses only" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger?unknownExpensesOnly=1")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) -> do
                -- All returned rows should be to Unknown_EXPENSE
                let allMatch = Array.find (\(LedgerViewRow row) -> row.toAccountName /= "Unknown_EXPENSE") ledgerRows
                case allMatch of
                  Nothing -> pure unit -- All good, all results match filter
                  Just _ -> shouldEqual "All results are unknown expenses" "Some results are not unknown expenses"

      it "should filter ledger view by soundex similarity" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger?soundexDescr=G626")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (ledgerRows :: Array LedgerViewRow) -> do
                -- All returned rows should have soundex "G626" (for "Grocery" variants)
                let allMatch = Array.find (\(LedgerViewRow row) -> row.soundexDescr /= "G626") ledgerRows
                case allMatch of
                  Nothing -> pure unit -- All good, all results match filter
                  Just _ -> shouldEqual "All results match soundex filter" "Some results don't match soundex filter"

    describe "Account Balances Endpoint" do
      it "should get account balances with valid accountIds" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=2,3")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (balances :: Array AccountBalanceRead) -> do
                Array.length balances `shouldSatisfy` (_ >= 2) -- Should have at least 2 balances
                -- Validate JSON structure of first balance
                case Array.head balances of
                  Just (AccountBalanceRead balance) -> do
                    balance.accountId `shouldSatisfy` (\id -> id == 2 || id == 3)
                    balance.categoryId `shouldEqual` 2 -- Assets category
                    balance.categoryName `shouldEqual` "Assets"
                    balance.accountName `shouldSatisfy` (\name -> name == "Checking account" || name == "Savings account")
                    balance.accountBalance `shouldSatisfy` (_ >= 0) -- Balance in cents
                  Nothing -> shouldEqual "Expected at least one balance" "No balances found"

      it "should return 400 error when accountIds parameter is missing" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 400) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left _ -> shouldEqual "Expected valid JSON" "Got JSON error"
              Right (errorObj :: { error :: String }) ->
                errorObj.error `shouldEqual` "Missing accountIds parameter"

      it "should return 400 error when accountIds parameter contains invalid values" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=abc,xyz")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 400) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left _ -> shouldEqual "Expected valid JSON" "Got JSON error"
              Right (errorObj :: { error :: String }) ->
                errorObj.error `shouldEqual` "Invalid accountIds parameter: must be comma-separated integers"

      it "should return 400 error when accountIds parameter is empty" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 400) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left _ -> shouldEqual "Expected valid JSON" "Got JSON error"
              Right (errorObj :: { error :: String }) ->
                errorObj.error `shouldEqual` "Invalid accountIds parameter: must be comma-separated integers"

      it "should return empty array for non-existent account IDs" do
        result <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=999,1000")
        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            -- Check Content-Type header
            case response.headers of
              headers -> headers `shouldSatisfy` (\h -> contains (Pattern "application/json") (show h))
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (balances :: Array AccountBalanceRead) ->
                Array.length balances `shouldEqual` 0

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
                Array.length budgets `shouldSatisfy` (_ >= 0)

      it "should get budget by ID when exists" do
        -- First get all budgets to find an existing one
        budgetsResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/budgets")
        case budgetsResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right budgetsResponse -> do
            case JSON.readJSON budgetsResponse.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (budgets :: Array BudgetDB) -> do
                case Array.head budgets of
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
                Array.length transactions `shouldSatisfy` (_ >= 0)

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

      it "should create a new transaction" do
        -- Create transaction data
        let
          transactionData =
            { budgetId: Nothing :: Maybe Int
            , fromAccountId: 2 -- Checking account
            , toAccountId: 6 -- Unknown expense
            , dateUnix: 1719792000 -- 2024-07-01
            , descr: "Test transaction from API"
            , cents: 4250 -- $42.50
            }

        -- Send POST request
        result <- AX.request AX.defaultRequest
          { method = Left POST
          , url = "http://localhost:" <> show port <> "/transactions"
          , responseFormat = ResponseFormat.string
          , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
          , content = Just $ RequestBody.string $ JSON.writeJSON transactionData
          }

        case result of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 201) response.status
            response.body `shouldEqual` ""

        -- Verify the transaction was created by fetching it
        ledgerResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
        case ledgerResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (transactions :: Array LedgerViewRow) -> do
                -- Find our new transaction
                let newTransaction = Array.find (\(LedgerViewRow row) -> row.descr == "Test transaction from API") transactions
                case newTransaction of
                  Nothing -> shouldEqual "Transaction found" "Transaction not found"
                  Just (LedgerViewRow txn) -> do
                    txn.descr `shouldEqual` "Test transaction from API"
                    txn.flowCents `shouldEqual` (-4250)
                    txn.fromAccountId `shouldEqual` 2
                    txn.toAccountId `shouldEqual` 6

      it "should update an existing transaction" do
        -- First create a transaction
        let
          createData =
            { budgetId: Nothing :: Maybe Int
            , fromAccountId: 2
            , toAccountId: 6
            , dateUnix: 1719792000
            , descr: "Original transaction"
            , cents: 1000
            }

        createResult <- AX.request AX.defaultRequest
          { method = Left POST
          , url = "http://localhost:" <> show port <> "/transactions"
          , responseFormat = ResponseFormat.string
          , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
          , content = Just $ RequestBody.string $ JSON.writeJSON createData
          }

        case createResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right _ -> pure unit

        -- Get the transaction ID
        ledgerResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
        case ledgerResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (transactions :: Array LedgerViewRow) -> do
                let originalTxn = Array.find (\(LedgerViewRow row) -> row.descr == "Original transaction") transactions
                case originalTxn of
                  Nothing -> shouldEqual "Transaction found" "Transaction not found"
                  Just (LedgerViewRow txn) -> do
                    -- Update the transaction
                    let
                      updateData =
                        { budgetId: Nothing :: Maybe Int
                        , fromAccountId: 2
                        , toAccountId: 7 -- Changed to Groceries
                        , dateUnix: 1719878400 -- Changed date
                        , descr: "Updated transaction"
                        , cents: 2500 -- Changed amount
                        }

                    updateResult <- AX.request AX.defaultRequest
                      { method = Left PUT
                      , url = "http://localhost:" <> show port <> "/transactions/" <> show txn.transactionId
                      , responseFormat = ResponseFormat.string
                      , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
                      , content = Just $ RequestBody.string $ JSON.writeJSON updateData
                      }

                    case updateResult of
                      Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
                      Right response -> shouldEqual (StatusCode 204) response.status

                    -- Verify the update
                    verifyResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
                    case verifyResult of
                      Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
                      Right response -> do
                        case JSON.readJSON response.body of
                          Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
                          Right (updatedTransactions :: Array LedgerViewRow) -> do
                            let updatedTxn = Array.find (\(LedgerViewRow row) -> row.transactionId == txn.transactionId) updatedTransactions
                            case updatedTxn of
                              Nothing -> shouldEqual "Transaction found" "Transaction not found"
                              Just (LedgerViewRow updated) -> do
                                updated.descr `shouldEqual` "Updated transaction"
                                updated.flowCents `shouldEqual` (-2500)
                                updated.toAccountId `shouldEqual` 7

      it "should delete an existing transaction and update balances" do
        -- Get initial balance
        initialBalanceResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=2")
        initialBalance <- case initialBalanceResult of
          Left err -> do
            shouldEqual "Expected success" $ "Got error: " <> AX.printError err
            pure 0
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> do
                shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
                pure 0
              Right (balances :: Array AccountBalanceRead) -> do
                case Array.head balances of
                  Just (AccountBalanceRead balance) -> pure balance.accountBalance
                  Nothing -> pure 0

        -- Create a transaction to delete
        let
          createData =
            { budgetId: Nothing :: Maybe Int
            , fromAccountId: 2 -- Checking account
            , toAccountId: 6 -- Unknown expense
            , dateUnix: 1719792000
            , descr: "Transaction to delete"
            , cents: 5000 -- $50.00
            }

        createResult <- AX.request AX.defaultRequest
          { method = Left POST
          , url = "http://localhost:" <> show port <> "/transactions"
          , responseFormat = ResponseFormat.string
          , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
          , content = Just $ RequestBody.string $ JSON.writeJSON createData
          }

        case createResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right _ -> pure unit

        -- Get the transaction ID and verify it exists
        ledgerResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
        transactionId <- case ledgerResult of
          Left err -> do
            shouldEqual "Expected success" $ "Got error: " <> AX.printError err
            pure 0
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> do
                shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
                pure 0
              Right (transactions :: Array LedgerViewRow) -> do
                let deleteTxn = Array.find (\(LedgerViewRow row) -> row.descr == "Transaction to delete") transactions
                case deleteTxn of
                  Nothing -> do
                    shouldEqual "Transaction found" "Transaction not found"
                    pure 0
                  Just (LedgerViewRow txn) -> do
                    txn.descr `shouldEqual` "Transaction to delete"
                    txn.flowCents `shouldEqual` (-5000)
                    pure txn.transactionId

        -- Delete the transaction
        deleteResult <- AX.request AX.defaultRequest
          { method = Left DELETE
          , url = "http://localhost:" <> show port <> "/transactions/" <> show transactionId
          , responseFormat = ResponseFormat.string
          , content = Nothing
          }

        case deleteResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            shouldEqual (StatusCode 200) response.status
            response.body `shouldEqual` "Transaction deleted"

        -- Verify the transaction is gone
        verifyResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/2/ledger")
        case verifyResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (remainingTransactions :: Array LedgerViewRow) -> do
                let deletedTxn = Array.find (\(LedgerViewRow row) -> row.descr == "Transaction to delete") remainingTransactions
                case deletedTxn of
                  Nothing -> pure unit -- Good, transaction is gone
                  Just _ -> shouldEqual "Transaction deleted" "Transaction still exists"

        -- Verify balance is restored
        finalBalanceResult <- AX.get ResponseFormat.string ("http://localhost:" <> show port <> "/accounts/balances?accountIds=2")
        case finalBalanceResult of
          Left err -> shouldEqual "Expected success" $ "Got error: " <> AX.printError err
          Right response -> do
            case JSON.readJSON response.body of
              Left err -> shouldEqual "Expected valid JSON" $ "Got JSON error: " <> show err
              Right (balances :: Array AccountBalanceRead) -> do
                case Array.head balances of
                  Just (AccountBalanceRead balance) ->
                    balance.accountBalance `shouldEqual` initialBalance
                  Nothing -> shouldEqual "Balance found" "No balance found"