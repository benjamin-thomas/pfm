module Test.Database.Spec
  ( spec
  ) where

import Prelude

import Data.Array (length, filter, (!!))
import Data.Maybe (Maybe(Just, Nothing))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import SQLite3 as SQLite3
import Server.DB.Account as Account
import Server.DB.Account (AccountDB(..))
import Server.DB.Budget as Budget
import Server.DB.Budget (BudgetDB(..))
import Server.DB.Category as Category
import Server.DB.Category (CategoryDB(..))
import Server.DB.Transaction as Transaction
import Server.DB.Transaction (TransactionNewRow(..))
import Server.Database as DB
import Shared.Types (Transaction(..), User(..))
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
        budgetId <- Budget.insertBudgetForDate 1719792000 db
        budgetId `shouldSatisfy` (_ > 0)

        -- Now retrieve it
        maybeBudgetId <- Budget.getBudgetIdForDate 1719792000 db
        maybeBudgetId `shouldEqual` Just budgetId

      it "should return Nothing for date without budget" do
        -- Use a date far in the future
        maybeBudgetId <- Budget.getBudgetIdForDate 2000000000 db
        maybeBudgetId `shouldEqual` Nothing

    describe "Category Operations" do
      it "should get all categories" do
        categories <- Category.getAllCategories db
        length categories `shouldEqual` 4

        -- Check first category (Equity)
        case categories !! 0 of
          Just (CategoryDB cat) -> do
            cat.categoryId `shouldEqual` 1
            cat.name `shouldEqual` "Equity"
          Nothing -> liftEffect $ throw "First category not found"

      it "should get category by ID" do
        maybeCategory <- Category.getCategoryById 2 db
        case maybeCategory of
          Just (CategoryDB cat) -> do
            cat.categoryId `shouldEqual` 2
            cat.name `shouldEqual` "Assets"
          Nothing -> liftEffect $ throw "Category 2 not found"

    describe "Account Operations" do
      it "should get all accounts" do
        accounts <- Account.getAllAccounts db
        length accounts `shouldSatisfy` (_ >= 13) -- We have at least 13 seeded accounts

        -- Check for checking account
        let checkingAccounts = filter (\(AccountDB acc) -> acc.name == "Checking account") accounts
        length checkingAccounts `shouldEqual` 1

    describe "Budget Operations" do
      it "should get all budgets after seeding" do
        -- Just insert a test budget instead of seeding entire OFX
        _ <- Budget.insertBudgetForDate 1719792000 db

        budgets <- Budget.getAllBudgets db
        length budgets `shouldSatisfy` (_ >= 1) -- Should have at least one budget after seeding

      it "should get budget by ID" do
        budgets <- Budget.getAllBudgets db
        case budgets !! 0 of
          Just (BudgetDB budget) -> do
            maybeBudget <- Budget.getBudgetById budget.budgetId db
            maybeBudget `shouldEqual` Just (BudgetDB budget)
          Nothing -> liftEffect $ throw "No budgets found"

    describe "Transaction Operations" do
      it "should insert and get transactions" do
        -- First ensure we have a budget
        budgetId <- Budget.insertBudgetForDate 1719792000 db

        let
          testTxn = TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 2 -- Checking account
            , toAccountId: 6 -- Unknown expense
            , uniqueFitId: Just "TEST123"
            , dateUnix: 1719792000
            , descrOrig: "Test Transaction"
            , descr: "Test Transaction"
            , cents: 1500
            }

        -- Insert test transaction
        Transaction.insertTransaction testTxn db

        transactions <- DB.getAllTransactions db
        length transactions `shouldSatisfy` (_ >= 1) -- Should have at least our test transaction

        -- Check that we have proper transaction structure
        case transactions !! 0 of
          Just (Transaction txn) -> do
            txn.id `shouldSatisfy` (_ > 0)
            txn.amount `shouldSatisfy` (_ > 0.0)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should get transaction by ID" do
        transactions <- DB.getAllTransactions db
        case transactions !! 0 of
          Just (Transaction txn) -> do
            maybeTransaction <- DB.getTransactionById txn.id db
            maybeTransaction `shouldEqual` Just (Transaction txn)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should update a transaction" do
        transactions <- DB.getAllTransactions db
        case transactions !! 0 of
          Just (Transaction txn) -> do
            let
              newTxnData = TransactionNewRow
                { budgetId: Just txn.budgetId
                , fromAccountId: txn.fromAccountId
                , toAccountId: txn.toAccountId
                , uniqueFitId: txn.uniqueFitId
                , dateUnix: 1719792000 -- Unix timestamp
                , descrOrig: txn.description
                , descr: "Updated Description" -- Changed this
                , cents: 9999 -- Changed this
                }

            -- Update the transaction
            Transaction.updateTransaction txn.id newTxnData db

            -- Verify it was updated
            maybeUpdated <- DB.getTransactionById txn.id db
            case maybeUpdated of
              Just (Transaction updated) -> do
                updated.description `shouldEqual` "Updated Description"
                updated.amount `shouldEqual` 99.99
              Nothing -> liftEffect $ throw "Updated transaction not found"
          Nothing -> liftEffect $ throw "No transactions found"

      it "should delete a transaction" do
        transactions1 <- DB.getAllTransactions db
        let initialCount = length transactions1

        case transactions1 !! 0 of
          Just (Transaction txn) -> do
            -- Delete the transaction
            Transaction.deleteTransaction txn.id db

            -- Verify it was deleted
            maybeDeleted <- DB.getTransactionById txn.id db
            maybeDeleted `shouldEqual` Nothing

            -- Verify count decreased
            transactions2 <- DB.getAllTransactions db
            length transactions2 `shouldEqual` (initialCount - 1)
          Nothing -> liftEffect $ throw "No transactions found"

