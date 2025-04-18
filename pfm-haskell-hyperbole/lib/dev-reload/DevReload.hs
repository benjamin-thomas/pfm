{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module DevReload (devReloadPageJs) where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Syntax (Loc (loc_filename), location)
import System.FilePath (takeDirectory, (</>))

devReloadPageJs :: Int -> Text
devReloadPageJs port =
    T.replace "__PORT__" (T.pack $ show port) jsTemplate
  where
    jsTemplate =
        decodeUtf8 $
            $( do
                loc <- location
                let dir = takeDirectory (loc_filename loc)
                    path = dir </> "devReloadPage.js"
                embedFile path
             )