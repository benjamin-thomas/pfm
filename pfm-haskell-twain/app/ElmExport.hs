{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ElmExport (exportElm) where

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
    '[ User
     , Category
     , LedgerLineSummary
     , TransactionWrite
     ]
