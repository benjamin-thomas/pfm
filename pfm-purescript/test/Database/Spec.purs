module Test.Database.Spec
  ( spec
  ) where

import Prelude

import Data.Array (length, filter, (!!))
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import SQLite3 as SQLite3
import Server.Database as DB
import Shared.Types (User(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: SQLite3.DBConnection -> Spec Unit
spec db = do
  describe "Database Integration Tests" do
    describe "User Operations" do
      it "should get one user" do
        maybeUser <- DB.getUserById 1 db
        maybeUser `shouldEqual`
          Just (User { id: Just 1, firstName: "John", lastName: "Doe" })
      it "should retrieve many users" do
        users <- DB.getAllUsers db
        users `shouldEqual`
          [ User { id: Just 1, firstName: "John", lastName: "Doe" }
          , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
          ]

      it "should insert a new user" do
        users1 <- DB.getAllUsers db
        users1 `shouldEqual`
          [ User { id: Just 1, firstName: "John", lastName: "Doe" }
          , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
          ]
        let newUser = User { id: Nothing, firstName: "Alice", lastName: "Johnson" }
        _ <- DB.insertUser newUser db

        users2 <- DB.getAllUsers db
        users2 `shouldEqual`
          [ User { id: Just 1, firstName: "John", lastName: "Doe" }
          , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
          , User { id: Just 3, firstName: "Alice", lastName: "Johnson" }
          ]

      it "should delete a user" do
        users1 <- DB.getAllUsers db
        users1 `shouldEqual`
          [ User { id: Just 1, firstName: "John", lastName: "Doe" }
          , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
          , User { id: Just 3, firstName: "Alice", lastName: "Johnson" }
          ]
        -- First get all users to find Alice's ID
        _ <- DB.deleteUser 3 db

        users2 <- DB.getAllUsers db
        users2 `shouldEqual`
          [ User { id: Just 1, firstName: "John", lastName: "Doe" }
          , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
          ]

    describe "Budget Operations (FIXME: theses tests are suboptimal for now)" do
      it "should get budget ID for a date" do
        -- Create a test budget first
        budgetId <- DB.insertBudgetForDate 1719792000 db
        budgetId `shouldSatisfy` (_ > 0)

        -- Now retrieve it
        maybeBudgetId <- DB.getBudgetIdForDate 1719792000 db
        maybeBudgetId `shouldEqual` Just budgetId

      it "should return Nothing for date without budget" do
        -- Use a date far in the future
        maybeBudgetId <- DB.getBudgetIdForDate 2000000000 db
        maybeBudgetId `shouldEqual` Nothing

    describe "Category Operations" do
      it "should get all categories" do
        categories <- DB.getAllCategories db
        length categories `shouldEqual` 4

        -- Check first category (Equity)
        case categories !! 0 of
          Just (DB.Category cat) -> do
            cat.categoryId `shouldEqual` 1
            cat.name `shouldEqual` "Equity"
          Nothing -> liftEffect $ throw "First category not found"

      it "should get category by ID" do
        maybeCategory <- DB.getCategoryById 2 db
        case maybeCategory of
          Just (DB.Category cat) -> do
            cat.categoryId `shouldEqual` 2
            cat.name `shouldEqual` "Assets"
          Nothing -> liftEffect $ throw "Category 2 not found"

    describe "Account Operations" do
      it "should get all accounts" do
        accounts <- DB.getAllAccounts db
        length accounts `shouldSatisfy` (_ >= 13) -- We have at least 13 seeded accounts

        -- Check for checking account
        let checkingAccounts = filter (\(DB.Account acc) -> acc.name == "Checking account") accounts
        length checkingAccounts `shouldEqual` 1

    describe "Budget Operations" do
      it "should get all budgets after seeding" do
        -- Just insert a test budget instead of seeding entire OFX
        _ <- DB.insertBudgetForDate 1719792000 db

        budgets <- DB.getAllBudgets db
        length budgets `shouldSatisfy` (_ >= 1) -- Should have at least one budget after seeding

      it "should get budget by ID" do
        budgets <- DB.getAllBudgets db
        case budgets !! 0 of
          Just (DB.Budget budget) -> do
            maybeBudget <- DB.getBudgetById budget.budgetId db
            maybeBudget `shouldEqual` Just (DB.Budget budget)
          Nothing -> liftEffect $ throw "No budgets found"

    describe "Transaction Operations" do
      it "should insert and get transactions" do
        -- First ensure we have a budget
        budgetId <- DB.insertBudgetForDate 1719792000 db

        let
          testTxn = DB.TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 2 -- Checking account
            , toAccountId: 6 -- Unknown expense
            , uniqueFitId: Just "TEST123"
            , date: 1719792000
            , descrOrig: "Test Transaction"
            , descr: "Test Transaction"
            , cents: 1500
            }

        -- Insert test transaction
        DB.insertTransaction testTxn db

        transactions <- DB.getAllTransactions db
        length transactions `shouldSatisfy` (_ >= 1) -- Should have at least our test transaction

        -- Check that we have proper transaction structure
        case transactions !! 0 of
          Just (DB.Transaction txn) -> do
            txn.transactionId `shouldSatisfy` (_ > 0)
            txn.cents `shouldSatisfy` (_ > 0)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should get transaction by ID" do
        transactions <- DB.getAllTransactions db
        case transactions !! 0 of
          Just (DB.Transaction txn) -> do
            maybeTransaction <- DB.getTransactionById txn.transactionId db
            maybeTransaction `shouldEqual` Just (DB.Transaction txn)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should update a transaction" do
        transactions <- DB.getAllTransactions db
        case transactions !! 0 of
          Just (DB.Transaction txn) -> do
            let
              newTxnData = DB.TransactionNewRow
                { budgetId: Just txn.budgetId
                , fromAccountId: txn.fromAccountId
                , toAccountId: txn.toAccountId
                , uniqueFitId: txn.uniqueFitId
                , date: txn.date
                , descrOrig: txn.descrOrig
                , descr: "Updated Description" -- Changed this
                , cents: 9999 -- Changed this
                }

            -- Update the transaction
            DB.updateTransaction txn.transactionId newTxnData db

            -- Verify it was updated
            maybeUpdated <- DB.getTransactionById txn.transactionId db
            case maybeUpdated of
              Just (DB.Transaction updated) -> do
                updated.descr `shouldEqual` "Updated Description"
                updated.cents `shouldEqual` 9999
              Nothing -> liftEffect $ throw "Updated transaction not found"
          Nothing -> liftEffect $ throw "No transactions found"

      it "should delete a transaction" do
        transactions1 <- DB.getAllTransactions db
        let initialCount = length transactions1

        case transactions1 !! 0 of
          Just (DB.Transaction txn) -> do
            -- Delete the transaction
            DB.deleteTransaction txn.transactionId db

            -- Verify it was deleted
            maybeDeleted <- DB.getTransactionById txn.transactionId db
            maybeDeleted `shouldEqual` Nothing

            -- Verify count decreased
            transactions2 <- DB.getAllTransactions db
            length transactions2 `shouldEqual` (initialCount - 1)
          Nothing -> liftEffect $ throw "No transactions found"

