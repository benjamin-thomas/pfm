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

    describe "Transaction Operations (FIXME: theses tests are suboptimal for now)" do
      it "should insert a transaction" do
        -- First ensure we have a budget
        budgetId <- DB.insertBudgetForDate 1719792000 db

        let
          txn = DB.TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 2 -- Checking account
            , toAccountId: 6 -- Unknown expense
            , uniqueFitId: Just "TEST123"
            , date: 1719792000
            , descrOrig: "Test Transaction"
            , descr: "Test Transaction"
            , cents: 1500
            }

        -- Should not throw
        DB.insertTransaction txn db

        -- Verify transaction was inserted (we'd need a getTransactions function for this)
        -- For now, just ensure no error was thrown
        pure unit

      it "should delete all transactions" do
        -- First insert a transaction
        budgetId <- DB.insertBudgetForDate 1719792001 db
        let
          txn = DB.TransactionNewRow
            { budgetId: Just budgetId
            , fromAccountId: 2
            , toAccountId: 6
            , uniqueFitId: Just "TEST456"
            , date: 1719792001
            , descrOrig: "Another Test"
            , descr: "Another Test"
            , cents: 2000
            }
        DB.insertTransaction txn db

        -- Now delete all
        DB.deleteAllTransactions db

        -- We'd need a way to verify this worked
        -- For now, just ensure no error was thrown
        pure unit

