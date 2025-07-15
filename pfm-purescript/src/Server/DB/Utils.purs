module Server.DB.Utils
  ( fromDbRows
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign (Foreign)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

-- | Safely decode database rows with proper error handling
-- | Takes a resource name for error messages, a conversion function, and the raw database rows
fromDbRows :: forall a b. ReadForeign a => String -> (a -> b) -> Foreign -> Aff (Array b)
fromDbRows resourceName rowToResource rows = case runExcept (JSON.readImpl rows) of
  Left err -> throwError $ error $ "Database decoding error for " <> resourceName <> ": " <> show err
  Right resourceRows -> pure $ map rowToResource resourceRows