{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ElmExport (exportElm) where

import DTO.AccountRead (AccountRead)
import DTO.Category (Category)
import DTO.Ledger (LedgerLineSummary)
import DTO.TransactionWrite
import DTO.User (User)
import Elm

exportElm :: IO ()
exportElm =
    generateElm @Types $
        defaultSettings "../pfm-elm/src/" ["Generated"]

type Types =
    '[ AccountRead
     , User
     , Category
     , LedgerLineSummary
     , TransactionWrite
     ]
