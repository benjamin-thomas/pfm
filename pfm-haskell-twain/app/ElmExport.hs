{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ElmExport (exportElm) where

import DTO.Category (CategoryDTO)
import DTO.User (UserDTO)
import Elm

exportElm :: IO ()
exportElm =
    generateElm @Types $
        defaultSettings "../pfm-elm/src/" ["Generated"]

type Types =
    '[ UserDTO
     , CategoryDTO
     --  , WithRunningBalanceDTO
     ]
