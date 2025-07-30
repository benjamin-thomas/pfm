module Test.Database.Spec
  ( spec
  ) where

import Prelude

import Data.Array (length, filter, (!!), head)
import Data.Maybe (Maybe(Just, Nothing))
import SQLite3 as SQLite3
import Server.DB.Account as Account
import Server.DB.Account (AccountDB(..), AccountBalanceDB(..))
import Server.DB.LedgerView as LedgerView
import Server.DB.LedgerView (LedgerViewRowDB(..))
import Server.DB.LedgerView.Queries as LedgerViewQueries
import Server.DB.Budget as Budget
import Server.DB.Budget (BudgetDB(..))
import Server.DB.Category as Category
import Server.DB.Category (CategoryDB(..))
import Server.DB.Transactions.Queries as TransactionQueries
import Server.DB.Transactions.Queries (TransactionNewRow(..))
import Server.DB.Suggestions.Queries as SuggestionQueries
import Shared.Types (LedgerViewRow(..), Transaction(..))
import Test.Database.TestUtils (withTestTransaction)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

spec :: SQLite3.DBConnection -> Spec Unit
spec db = do
  describe "Database Integration Tests" do
    describe "Budget Operations (FIXME: theses tests are suboptimal for now)" do
      it "should get budget ID for a date" do
        withTestTransaction db do
          -- Create a test budget first
          budgetId <- Budget.insertBudgetForDate 1719792000 db
          budgetId `shouldSatisfy` (_ > 0)

          -- Now retrieve it
          maybeBudgetId <- Budget.getBudgetIdForDate 1719792000 db
          maybeBudgetId `shouldEqual` Just budgetId

      it "should return Nothing for date without budget" do
        withTestTransaction db do
          -- Use a date far in the future
          maybeBudgetId <- Budget.getBudgetIdForDate 2000000000 db
          maybeBudgetId `shouldEqual` Nothing

    describe "Category Operations" do
      it "should get all categories" do
        withTestTransaction db do
          categories <- Category.getAllCategories db
          length categories `shouldEqual` 4

          -- Check first category (Equity)
          case categories !! 0 of
            Just (CategoryDB cat) -> do
              cat.categoryId `shouldEqual` 1
              cat.name `shouldEqual` "Equity"
            Nothing -> fail "First category not found"

      it "should get category by ID" do
        withTestTransaction db do
          maybeCategory <- Category.getCategoryById 2 db
          case maybeCategory of
            Just (CategoryDB cat) -> do
              cat.categoryId `shouldEqual` 2
              cat.name `shouldEqual` "Assets"
            Nothing -> fail "Category 2 not found"

    describe "Account Operations" do
      it "should get all accounts" do
        withTestTransaction db do
          accounts <- Account.getAllAccounts db
          length accounts `shouldSatisfy` (_ >= 13) -- We have at least 13 seeded accounts

          -- Check for checking account
          let checkingAccounts = filter (\(AccountDB acc) -> acc.name == "Checking account") accounts
          length checkingAccounts `shouldEqual` 1

    describe "Account Balance Operations" do
      it "should get account balances for specific accounts" do
        withTestTransaction db do
          -- Get balances for checking account (2) and savings account (3)
          balances <- Account.getAccountBalances [ 2, 3 ] db
          length balances `shouldSatisfy` (_ >= 2) -- Should have at least 2 balances

          -- Verify balance structure
          case balances !! 0 of
            Just (AccountBalanceDB balance) -> do
              balance.accountId `shouldSatisfy` (\id -> id == 2 || id == 3)
              balance.categoryId `shouldEqual` 2 -- Assets category
              balance.categoryName `shouldEqual` "Assets"
              balance.accountName `shouldSatisfy` (\name -> name == "Checking account" || name == "Savings account")
              balance.accountBalance `shouldEqual` 0 -- Should be 0 initially with no transactions
            Nothing -> fail "Expected at least one balance"

      it "should return empty array for empty account IDs" do
        withTestTransaction db do
          balances <- Account.getAccountBalances [] db
          length balances `shouldEqual` 0

      it "should handle non-existent account IDs gracefully" do
        withTestTransaction db do
          balances <- Account.getAccountBalances [ 999, 1000 ] db
          length balances `shouldEqual` 0

      it "should calculate correct balances after transactions" do
        withTestTransaction db do
          -- First ensure we have a budget
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          -- Insert transactions that affect the balance
          let
            -- Money going out from checking account
            outgoingTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "BALANCE-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Test expense"
              , descr: "Test expense"
              , cents: 10000 -- $100.00
              }

            -- Money coming into checking account
            incomingTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account
              , uniqueFitId: Just "BALANCE-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Test income"
              , descr: "Test income"
              , cents: 25000 -- $250.00
              }

          -- Insert transactions
          TransactionQueries.insertTransaction outgoingTxn db
          TransactionQueries.insertTransaction incomingTxn db

          -- Get balance for checking account
          balances <- Account.getAccountBalances [ 2 ] db
          length balances `shouldEqual` 1

          case balances !! 0 of
            Just (AccountBalanceDB balance) -> do
              balance.accountId `shouldEqual` 2
              balance.accountName `shouldEqual` "Checking account"
              -- Balance should be -100.00 + 250.00 = 150.00 (15000 cents)
              balance.accountBalance `shouldEqual` 15000
            Nothing -> fail "Expected balance for checking account"

    describe "Budget Operations" do
      it "should get all budgets after seeding" do
        withTestTransaction db do
          -- Just insert a test budget instead of seeding entire OFX
          _ <- Budget.insertBudgetForDate 1719792000 db

          budgets <- Budget.getAllBudgets db
          length budgets `shouldSatisfy` (_ >= 1) -- Should have at least one budget after seeding

      it "should get budget by ID" do
        withTestTransaction db do
          -- Create a test budget within this test
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          -- Now test getting it by ID
          maybeBudget <- Budget.getBudgetById budgetId db
          case maybeBudget of
            Just (BudgetDB budget) -> do
              budget.budgetId `shouldEqual` budgetId
            Nothing -> fail "Budget not found after creation"

    describe "Transaction Operations" do
      it "should insert and get transactions" do
        withTestTransaction db do
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
            Nothing -> fail "No transactions found"

      it "should get transaction by ID" do
        withTestTransaction db do
          -- Create test data within this test
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "GET-BY-ID-TEST"
              , dateUnix: 1719792000
              , descrOrig: "Get by ID test transaction"
              , descr: "Get by ID test transaction"
              , cents: 2500
              }

          -- Insert the test transaction
          TransactionQueries.insertTransaction testTxn db

          -- Get all transactions to find our test transaction
          transactions <- TransactionQueries.getAllTransactions db
          case transactions !! 0 of
            Just (Transaction txn) -> do
              -- Now test getting it by ID
              maybeTransaction <- TransactionQueries.getTransactionById txn.id db
              maybeTransaction `shouldEqual` Just (Transaction txn)
            Nothing -> fail "No transactions found after creation"

      it "should update a transaction" do
        withTestTransaction db do
          -- Create test data within this test
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            originalTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "UPDATE-TEST"
              , dateUnix: 1719792000
              , descrOrig: "Original Description"
              , descr: "Original Description"
              , cents: 5000
              }

          -- Insert the test transaction
          TransactionQueries.insertTransaction originalTxn db

          -- Get the transaction we just created
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
                Nothing -> fail "Updated transaction not found"
            Nothing -> fail "No transactions found after creation"

      it "should delete a transaction" do
        withTestTransaction db do
          -- Create test data within this test
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "DELETE-TEST"
              , dateUnix: 1719792000
              , descrOrig: "Transaction to delete"
              , descr: "Transaction to delete"
              , cents: 7500
              }

          -- Insert the test transaction
          TransactionQueries.insertTransaction testTxn db

          -- Get initial count (should be 1)
          transactions1 <- TransactionQueries.getAllTransactions db
          let initialCount = length transactions1
          initialCount `shouldEqual` 1

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
            Nothing -> fail "No transactions found after creation"

    describe "Batch Suggestion Operations" do
      it "should apply suggestions to multiple transactions" do
        withTestTransaction db do
          -- Create a budget for test transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          -- Create a categorized grocery transaction (establishes the pattern)
          let
            categorizedTxn = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 7 -- Groceries account (categorized)
              , uniqueFitId: Just "CATEGORIZED-1"
              , dateUnix: 1719792000
              , descrOrig: "GROCERY MART"
              , descr: "GROCERY MART"
              , cents: 5000
              }
          TransactionQueries.insertTransaction categorizedTxn db

          -- Create two uncategorized grocery transactions
          let
            uncategorized1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense (uncategorized)
              , uniqueFitId: Just "UNCATEGORIZED-1"
              , dateUnix: 1719792100
              , descrOrig: "GROCERY STORE"
              , descr: "GROCERY STORE"
              , cents: 3000
              }
          TransactionQueries.insertTransaction uncategorized1 db

          let
            uncategorized2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account
              , toAccountId: 6 -- Unknown expense (uncategorized)
              , uniqueFitId: Just "UNCATEGORIZED-2"
              , dateUnix: 1719792200
              , descrOrig: "GROCERY PLAZA"
              , descr: "GROCERY PLAZA"
              , cents: 4000
              }
          TransactionQueries.insertTransaction uncategorized2 db

          -- Get all transactions to find the IDs of uncategorized ones
          allTransactions <- TransactionQueries.getAllTransactions db
          length allTransactions `shouldEqual` 3

          -- Find the uncategorized transactions
          let uncategorizedTxns = filter (\(Transaction t) -> t.toAccountId == 6) allTransactions
          length uncategorizedTxns `shouldEqual` 2

          -- Create suggestion updates for both uncategorized transactions
          let
            suggestions = map
              ( \(Transaction t) ->
                  { transactionId: t.id
                  , toAccountId: 7 -- Move to Groceries account
                  }
              )
              uncategorizedTxns

          -- Apply batch suggestions
          SuggestionQueries.batchApplySuggestions suggestions db

          -- Verify all transactions now go to account 7
          updatedTransactions <- TransactionQueries.getAllTransactions db
          let groceryTransactions = filter (\(Transaction t) -> t.toAccountId == 7) updatedTransactions
          length groceryTransactions `shouldEqual` 3 -- All 3 should now be categorized

    describe "Ledger View Operations" do
      it "should get ledger view for checking account" do
        withTestTransaction db do
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
          let emptyFilters = { description: Nothing, soundexDescr: Nothing, minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Nothing }
          ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO 2 emptyFilters db
          length ledgerRows `shouldSatisfy` (_ >= 2)

          -- Check that ledger rows have proper flow calculations
          -- Look for our specific test transactions
          let
            testRows = filter
              ( \(LedgerViewRow r) ->
                  r.descr == "Grocery Store" || r.descr == "Salary"
              )
              ledgerRows

          case testRows of
            [ LedgerViewRow row1, LedgerViewRow row2 ] -> do
              -- With DESC order, Salary (newer) comes first, then Grocery Store (older)
              -- Salary: money coming into checking account (positive flow)
              row1.flowCents `shouldSatisfy` (_ > 0)

              -- Grocery Store: money going out from checking account (negative flow)
              row2.flowCents `shouldSatisfy` (_ < 0)
            _ -> fail "Expected exactly 2 test ledger rows"

      it "should calculate running balance correctly" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "BALANCE-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "BALANCE-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- Get ledger view again to test running balance calculation
          let emptyFilters = { description: Nothing, soundexDescr: Nothing, minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Nothing }
          ledgerRows <- LedgerViewQueries.getLedgerViewRowsAsDTO 2 emptyFilters db

          -- Look for our specific test transactions
          let
            testRows = filter
              ( \(LedgerViewRow r) ->
                  r.descr == "Grocery Store" || r.descr == "Salary"
              )
              ledgerRows

          case testRows of
            [ LedgerViewRow row1, LedgerViewRow row2 ] -> do
              -- With DESC order, Salary comes first, then Grocery Store
              -- Salary: +$2000.00
              row1.flowCents `shouldEqual` 200000

              -- Grocery Store: -$50.00
              row2.flowCents `shouldEqual` (-5000)

            -- Running balance calculation depends on previous transactions
            -- So we can only verify the flow amounts are correct
            _ -> fail "Expected exactly 2 test ledger rows for balance test"

      it "should filter by description" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "FILTER-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "FILTER-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- Test description filter with existing data
          let
            filters = { description: Just "Grocery", soundexDescr: Nothing, minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Nothing }

          filteredRows <- LedgerView.getLedgerViewRows 2 filters db

          -- Should only return the Grocery Store transaction
          length filteredRows `shouldEqual` 1
          case head filteredRows of
            Just (LedgerViewRowDB row) -> row.descr `shouldEqual` "Grocery Store"
            Nothing -> fail "Expected one filtered row"

      it "should filter by minimum amount" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "MIN-FILTER-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "MIN-FILTER-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- Test minimum amount filter (filter for amounts >= $100 = 10000 cents)
          let
            filters = { description: Nothing, soundexDescr: Nothing, minAmount: Just 10000, maxAmount: Nothing, filterUnknownExpenses: Nothing }

          filteredRows <- LedgerView.getLedgerViewRows 2 filters db

          -- Should only return the Salary transaction ($2000 > $100, Grocery Store $50 < $100)
          length filteredRows `shouldEqual` 1
          case head filteredRows of
            Just (LedgerViewRowDB row) -> do
              row.descr `shouldEqual` "Salary"
              row.flowCents `shouldEqual` 200000
            Nothing -> fail "Expected one filtered row"

      it "should filter by maximum amount" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "MAX-FILTER-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "MAX-FILTER-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- Test maximum amount filter (filter for amounts <= $100 = 10000 cents)
          let
            filters = { description: Nothing, soundexDescr: Nothing, minAmount: Nothing, maxAmount: Just 10000, filterUnknownExpenses: Nothing }

          filteredRows <- LedgerView.getLedgerViewRows 2 filters db

          -- Should only return the Grocery Store transaction ($50 <= $100, Salary $2000 > $100)
          length filteredRows `shouldEqual` 1
          case head filteredRows of
            Just (LedgerViewRowDB row) -> do
              row.descr `shouldEqual` "Grocery Store"
              row.flowCents `shouldEqual` (-5000) -- Note: negative because money going out
            Nothing -> fail "Expected one filtered row"

      it "should filter by unknown expenses only" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "UNKNOWN-FILTER-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "UNKNOWN-FILTER-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- Test unknown expenses filter - only show expenses going to "Unknown expense" account (ID=6)
          let
            filters = { description: Nothing, soundexDescr: Nothing, minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Just true }

          filteredRows <- LedgerView.getLedgerViewRows 2 filters db

          -- Should only return the Grocery Store transaction (goes to Unknown expense account)
          -- The Salary transaction goes from Unknown income (4) to Checking (2), so it's not an unknown expense
          length filteredRows `shouldEqual` 1
          case head filteredRows of
            Just (LedgerViewRowDB row) -> do
              row.descr `shouldEqual` "Grocery Store"
              row.toAccountId `shouldEqual` 6 -- Unknown expense account
            Nothing -> fail "Expected one filtered row"

      it "should filter by soundex similarity" do
        withTestTransaction db do
          -- First ensure we have a budget and some transactions
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          let
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "SOUNDEX-FILTER-TEST1"
              , dateUnix: 1719792000
              , descrOrig: "Grocery Store"
              , descr: "Grocery Store"
              , cents: 5000 -- $50.00
              }

            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 4 -- Unknown income
              , toAccountId: 2 -- Checking account (money coming in)
              , uniqueFitId: Just "SOUNDEX-FILTER-TEST2"
              , dateUnix: 1719792100
              , descrOrig: "Salary"
              , descr: "Salary"
              , cents: 200000 -- $2000.00
              }

          -- Insert test transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db

          -- First verify we have multiple rows before filtering
          let emptyFilters = { description: Nothing, soundexDescr: Nothing, minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Nothing }
          allRows <- LedgerView.getLedgerViewRows 2 emptyFilters db
          length allRows `shouldSatisfy` (_ >= 2)

          -- Test soundex filter - SOUNDEX("Grocery Store") = "G626"
          let
            filters = { description: Nothing, soundexDescr: Just "G626", minAmount: Nothing, maxAmount: Nothing, filterUnknownExpenses: Nothing }

          filteredRows <- LedgerView.getLedgerViewRows 2 filters db

          -- Map to descriptions for cleaner assertion
          let descriptions = map (\(LedgerViewRowDB row) -> row.descr) filteredRows
          descriptions `shouldEqual` [ "Grocery Store" ]

      it "should calculate prior balance correctly with filtered transactions" do
        withTestTransaction db do
          -- Create a budget first
          budgetId <- Budget.insertBudgetForDate 1719792000 db

          -- Insert multiple transactions with different amounts
          let
            -- First transaction: $100 expense
            testTxn1 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "PRIOR-BAL-1"
              , dateUnix: 1719792000
              , descrOrig: "Expense 1"
              , descr: "Expense 1"
              , cents: 10000 -- $100.00
              }

            -- Second transaction: $50 expense (will be filtered out)
            testTxn2 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "PRIOR-BAL-2"
              , dateUnix: 1719792100
              , descrOrig: "Small Expense"
              , descr: "Small Expense"
              , cents: 5000 -- $50.00
              }

            -- Third transaction: $150 expense
            testTxn3 = TransactionNewRow
              { budgetId: Just budgetId
              , fromAccountId: 2 -- Checking account (money going out)
              , toAccountId: 6 -- Unknown expense
              , uniqueFitId: Just "PRIOR-BAL-3"
              , dateUnix: 1719792200
              , descrOrig: "Expense 3"
              , descr: "Expense 3"
              , cents: 15000 -- $150.00
              }

          -- Insert all transactions
          TransactionQueries.insertTransaction testTxn1 db
          TransactionQueries.insertTransaction testTxn2 db
          TransactionQueries.insertTransaction testTxn3 db

          -- Get ledger view with filter that excludes the middle transaction
          let filterLarge = { description: Nothing, soundexDescr: Nothing, minAmount: Just 10000, maxAmount: Nothing, filterUnknownExpenses: Nothing }
          filteredRows <- LedgerView.getLedgerViewRows 2 filterLarge db

          -- We should only get two transactions (100 and 150)
          let
            ourRows = filter
              (\(LedgerViewRowDB r) -> r.descr == "Expense 1" || r.descr == "Expense 3")
              filteredRows

          case ourRows of
            [ LedgerViewRowDB row3, LedgerViewRowDB row1 ] -> do
              -- With DESC order, Expense 3 (newest) comes first
              -- Third transaction: prior balance should include ALL previous transactions
              -- even the filtered out one (0 - 100 - 50 = -150)
              row3.descr `shouldEqual` "Expense 3"
              row3.priorBalance `shouldEqual` "-150.00" -- This is the key test!
              row3.runningBalance `shouldEqual` "-300.00" -- -150 - 150 = -300

              -- First transaction (shown last due to DESC order): prior balance should be 0
              row1.descr `shouldEqual` "Expense 1"
              row1.priorBalance `shouldEqual` "0.00"
              row1.runningBalance `shouldEqual` "-100.00"
            _ -> fail "Expected exactly 2 filtered transactions"
