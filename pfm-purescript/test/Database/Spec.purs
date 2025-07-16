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
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Budget as Budget
import Server.DB.Budget (BudgetDB(..))
import Server.DB.Category as Category
import Server.DB.Category (CategoryDB(..))
import Server.DB.Transactions.Queries as TransactionQueries
import Server.DB.Transactions.Queries (TransactionNewRow(..))
import Server.Database as DB
import Shared.Types (LedgerViewRow(..), Transaction(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: SQLite3.DBConnection -> Spec Unit
spec db = do
  describe "Database Integration Tests" do
    -- describe "User Operations" do
    --   it "should get one user" do
    --     maybeUser <- DB.getUserById 1 db
    --     maybeUser `shouldEqual`
    --       Just (User { id: Just 1, firstName: "John", lastName: "Doe" })
    --   it "should retrieve many users" do
    --     users <- DB.getAllUsers db
    --     users `shouldEqual`
    --       [ User { id: Just 1, firstName: "John", lastName: "Doe" }
    --       , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
    --       ]

    --   it "should insert a new user" do
    --     users1 <- DB.getAllUsers db
    --     users1 `shouldEqual`
    --       [ User { id: Just 1, firstName: "John", lastName: "Doe" }
    --       , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
    --       ]
    --     let newUser = User { id: Nothing, firstName: "Alice", lastName: "Johnson" }
    --     _ <- DB.insertUser newUser db

    --     users2 <- DB.getAllUsers db
    --     users2 `shouldEqual`
    --       [ User { id: Just 1, firstName: "John", lastName: "Doe" }
    --       , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
    --       , User { id: Just 3, firstName: "Alice", lastName: "Johnson" }
    --       ]

    --   it "should delete a user" do
    --     users1 <- DB.getAllUsers db
    --     users1 `shouldEqual`
    --       [ User { id: Just 1, firstName: "John", lastName: "Doe" }
    --       , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
    --       , User { id: Just 3, firstName: "Alice", lastName: "Johnson" }
    --       ]
    --     -- First get all users to find Alice's ID
    --     _ <- DB.deleteUser 3 db

    --     users2 <- DB.getAllUsers db
    --     users2 `shouldEqual`
    --       [ User { id: Just 1, firstName: "John", lastName: "Doe" }
    --       , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
    --       ]

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
        TransactionQueries.insertTransaction testTxn db

        transactions <- TransactionQueries.getAllTransactions db
        length transactions `shouldSatisfy` (_ >= 1) -- Should have at least our test transaction

        -- Check that we have proper transaction structure
        case transactions !! 0 of
          Just (Transaction txn) -> do
            txn.id `shouldSatisfy` (_ > 0)
            txn.amount `shouldSatisfy` (_ > 0.0)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should get transaction by ID" do
        transactions <- TransactionQueries.getAllTransactions db
        case transactions !! 0 of
          Just (Transaction txn) -> do
            maybeTransaction <- TransactionQueries.getTransactionById txn.id db
            maybeTransaction `shouldEqual` Just (Transaction txn)
          Nothing -> liftEffect $ throw "No transactions found"

      it "should update a transaction" do
        transactions <- TransactionQueries.getAllTransactions db
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
            TransactionQueries.updateTransaction txn.id newTxnData db

            -- Verify it was updated
            maybeUpdated <- TransactionQueries.getTransactionById txn.id db
            case maybeUpdated of
              Just (Transaction updated) -> do
                updated.description `shouldEqual` "Updated Description"
                updated.amount `shouldEqual` 99.99
              Nothing -> liftEffect $ throw "Updated transaction not found"
          Nothing -> liftEffect $ throw "No transactions found"

      it "should delete a transaction" do
        transactions1 <- TransactionQueries.getAllTransactions db
        let initialCount = length transactions1

        case transactions1 !! 0 of
          Just (Transaction txn) -> do
            -- Delete the transaction
            TransactionQueries.deleteTransaction txn.id db

            -- Verify it was deleted
            maybeDeleted <- TransactionQueries.getTransactionById txn.id db
            maybeDeleted `shouldEqual` Nothing

            -- Verify count decreased
            transactions2 <- TransactionQueries.getAllTransactions db
            length transactions2 `shouldEqual` (initialCount - 1)
          Nothing -> liftEffect $ throw "No transactions found"

    describe "Ledger View Operations" do
      it "should get ledger view for checking account" do
        -- First ensure we have a budget and some transactions
        budgetId <- Budget.insertBudgetForDate 1719792000 db

        let
          testTxn1 = TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 2 -- Checking account (money going out)
            , toAccountId: 6 -- Unknown expense
            , uniqueFitId: Just "LEDGER-TEST1"
            , dateUnix: 1719792000
            , descrOrig: "Grocery Store"
            , descr: "Grocery Store"
            , cents: 5000 -- $50.00
            }

          testTxn2 = TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 4 -- Unknown income
            , toAccountId: 2 -- Checking account (money coming in)
            , uniqueFitId: Just "LEDGER-TEST2"
            , dateUnix: 1719792100
            , descrOrig: "Salary"
            , descr: "Salary"
            , cents: 200000 -- $2000.00
            }

        -- Insert test transactions
        TransactionQueries.insertTransaction testTxn1 db
        TransactionQueries.insertTransaction testTxn2 db

        -- Get ledger view for checking account (ID = 2)
        ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO 2 db
        length ledgerRows `shouldSatisfy` (_ >= 2)

        -- Check that ledger rows have proper flow calculations
        case ledgerRows of
          [ LedgerViewRow row1, LedgerViewRow row2 ] -> do
            -- First transaction: money going out from checking account (negative flow)
            row1.flowAmount `shouldSatisfy` (_ < 0.0)
            row1.runningBalance `shouldSatisfy` (_ < 0.0)

            -- Second transaction: money coming into checking account (positive flow)
            row2.flowAmount `shouldSatisfy` (_ > 0.0)
            row2.runningBalance `shouldSatisfy` (_ > row1.runningBalance)
          _ -> liftEffect $ throw "Expected exactly 2 ledger rows"

      it "should calculate running balance correctly" do
        -- Get ledger view again to test running balance calculation
        ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO 2 db

        case ledgerRows of
          [ LedgerViewRow row1, LedgerViewRow row2 ] -> do
            -- First row: -$50.00, running balance should be -$50.00
            row1.flowAmount `shouldEqual` (-50.0)
            row1.runningBalance `shouldEqual` (-50.0)

            -- Second row: +$2000.00, running balance should be $1950.00
            row2.flowAmount `shouldEqual` 2000.0
            row2.runningBalance `shouldEqual` 1950.0
          _ -> liftEffect $ throw "Expected exactly 2 ledger rows for balance test"

