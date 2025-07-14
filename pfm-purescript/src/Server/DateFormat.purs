module Server.DateFormat
  ( formatUnixTimestamp
  ) where

-- | Format a Unix timestamp to ISO date string (YYYY-MM-DD)
foreign import formatUnixTimestamp :: Int -> String