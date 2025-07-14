module Server.Conversion
  ( dbTransactionToTransaction
  ) where

import Prelude

import Data.Int (toNumber)
import Server.DateFormat (formatUnixTimestamp)
import Server.DB.Transaction (TransactionDB(..))
import Shared.Types (Transaction(..))

-- | Convert database transaction to DTO
dbTransactionToTransaction :: TransactionDB -> Transaction
dbTransactionToTransaction (TransactionDB dbTx) =
  Transaction
    { id: dbTx.transactionId
    , budgetId: dbTx.budgetId
    , fromAccountId: dbTx.fromAccountId
    , fromAccountName: getAccountName dbTx.fromAccountId
    , toAccountId: dbTx.toAccountId
    , toAccountName: getAccountName dbTx.toAccountId
    , uniqueFitId: dbTx.uniqueFitId
    , date: formatUnixTimestamp dbTx.dateUnix
    , description: dbTx.descr
    , amount: centsToNumber dbTx.cents
    }
  where
  centsToNumber :: Int -> Number
  centsToNumber cents = (toNumber cents) / 100.0
  
  
  -- Simple lookup for account names - in a real app this would come from the API
  getAccountName :: Int -> String
  getAccountName accountId = case accountId of
    1 -> "OpeningBalance"
    2 -> "Checking account"
    3 -> "Savings account" 
    4 -> "Unknown_INCOME"
    5 -> "Employer"
    6 -> "Unknown_EXPENSE"
    7 -> "Groceries"
    8 -> "Communications"
    9 -> "Transport"
    10 -> "Health"
    11 -> "Energy"
    12 -> "Clothing"
    13 -> "Leisure"
    _ -> "Unknown Account"