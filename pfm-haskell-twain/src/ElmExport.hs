{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ElmExport (exportElm) where

import DB.Budgets.JSON
    ( BudgetJSON
    , BudgetLineJSON
    , BudgetWithLinesJSON
    )
import DTO.AccountRead (AccountBalanceRead, AccountRead)
import DTO.Category (Category)
import DTO.Ledger (LedgerLine, SuggestedAccount, Suggestion)
import DTO.TransactionWrite
import DTO.User (User)
import Elm

exportElm :: IO ()
exportElm =
    generateElm @Types $
        defaultSettings "../pfm-elm/src/" ["Generated"]

type Types =
    '[ AccountRead
     , AccountBalanceRead
     , User
     , Category
     , LedgerLine
     , TransactionWrite
     , Suggestion
     , SuggestionWrite
     , SuggestedAccount
     , BudgetJSON
     , BudgetLineJSON
     , BudgetWithLinesJSON
     ]
