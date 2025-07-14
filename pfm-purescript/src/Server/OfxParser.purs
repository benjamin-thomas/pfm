module Server.OfxParser
  ( OfxBatch(..)
  , StatementTransaction(..)
  , TimeStamp(..)
  , parseOfx
  ) where

import Prelude

import Data.Array as Array
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as StringCU
import Data.Time (Time(..))
import Data.Traversable (sequence)
import Parsing (ParserT, fail, runParser)
import Parsing.Combinators (lookAhead, many, manyTill, option, try, (<|>))
import Parsing.String (anyChar, char, satisfy, string)
import Parsing.String.Basic (digit, space)

-- | Represents a timestamp that can be either a full DateTime or just a Date
data TimeStamp
  = FullDate DateTime
  | ShortDate Date

derive instance eqTimeStamp :: Eq TimeStamp

instance showTimeStamp :: Show TimeStamp where
  show (FullDate dt) = "(FullDate " <> show dt <> ")"
  show (ShortDate d) = "(ShortDate " <> show d <> ")"

{-| Represents a single transaction from an OFX file

Source: https://financialdataexchange.org/common/Uploaded%20files/OFX%20files/OFX%20Banking%20Specification%20v2.3.pdf

3.2.3 Financial Institution Transaction ID <FITID>

If a client performs the same type of request
within the same scope at two different FIs, clients will need to use FI + <ACCTID> + <FITID> as a
globally unique key in a client database. That is, the <FITID> value must be unique within the account and
Financial Institution (independent of the service provider).

 -}
type StatementTransaction =
  { posted :: TimeStamp
  , amount :: Decimal -- Amount with 2 decimal places (e.g., 123.45)
  , fitId :: String -- Financial Institution Transaction ID
  , name :: String
  , memo :: String
  }

-- | Represents a batch of transactions with an account number
type OfxBatch =
  { accountNumber :: String
  , transactions :: Array StatementTransaction
  }

-- | Skip whitespace
ws :: forall m. Monad m => ParserT String m Unit
ws = void $ many space

-- | Parse a symbol with optional whitespace
symbol :: forall m. Monad m => String -> ParserT String m String
symbol s = ws *> string s <* ws

-- | Parse tag value (everything until newline, > or <)
tagValueParser :: forall m. Monad m => ParserT String m String
tagValueParser = do
  chars <- many (satisfy (\c -> c /= '\n' && c /= '>' && c /= '<'))
  if List.null chars then fail "Expected at least one character"
  else pure $ StringCU.fromCharArray (Array.fromFoldable chars)

-- | Parse exactly n digits
count :: forall m a. Monad m => Int -> ParserT String m a -> ParserT String m (Array a)
count n p = Array.fromFoldable <$> sequence (Array.replicate n p)

-- | Parse a short date (YYYYMMDD)
shortDateParser :: forall m. Monad m => ParserT String m Date
shortDateParser = do
  year <- readDigits 4
  month <- readDigits 2
  day <- readDigits 2
  case toEnum year, toEnum month, toEnum day of
    Just y, Just m, Just d -> pure $ canonicalDate y m d
    _, _, _ -> fail "Invalid date"
  where
  readDigits n = do
    digits <- count n digit
    case Int.fromString (StringCU.fromCharArray digits) of
      Just i -> pure i
      Nothing -> fail "Invalid number"

-- | Parse a full date with time (YYYYMMDDHHmmss.SSS)
fullDateParser :: forall m. Monad m => ParserT String m DateTime
fullDateParser = do
  year <- readDigits 4
  month <- readDigits 2
  day <- readDigits 2
  hour <- readDigits 2
  minute <- readDigits 2
  second <- readDigits 2
  _ <- char '.'
  milliseconds <- readDigits 3

  case toEnum year, toEnum month, toEnum day, toEnum hour, toEnum minute, toEnum second, toEnum milliseconds of
    Just y, Just mo, Just d, Just h, Just mi, Just s, Just ms ->
      pure $ DateTime (canonicalDate y mo d) (Time h mi s ms)
    _, _, _, _, _, _, _ -> fail "Invalid datetime"
  where
  readDigits n = do
    digits <- count n digit
    case Int.fromString (StringCU.fromCharArray digits) of
      Just i -> pure i
      Nothing -> fail "Invalid number"

-- | Parse DTPOSTED tag
parseDtPosted :: forall m. Monad m => ParserT String m TimeStamp
parseDtPosted = do
  _ <- symbol "<DTPOSTED>"
  try (FullDate <$> fullDateParser) <|> (ShortDate <$> shortDateParser)

-- | Parse transaction amount (creates a Decimal with 2 decimal places)
transactionAmountParser :: forall m. Monad m => ParserT String m Decimal
transactionAmountParser = do
  _ <- symbol "<TRNAMT>"
  sign <- option '+' (char '-' <|> char '+')
  intPart <- many digit
  _ <- char '.'
  fracPart <- count 2 digit

  let intStr = StringCU.fromCharArray (Array.fromFoldable intPart)
  let fracStr = StringCU.fromCharArray fracPart
  let amountStr = intStr <> "." <> fracStr
  let signedAmountStr = if sign == '-' then "-" <> amountStr else amountStr

  case Decimal.fromString signedAmountStr of
    Just decimal -> pure decimal
    Nothing -> fail "Invalid amount"

-- | Parse FITID tag
fitIdParser :: forall m. Monad m => ParserT String m String
fitIdParser = symbol "<FITID>" *> tagValueParser

-- | Parse NAME tag
nameParser :: forall m. Monad m => ParserT String m String
nameParser = symbol "<NAME>" *> tagValueParser

-- | Parse MEMO tag
memoParser :: forall m. Monad m => ParserT String m String
memoParser = symbol "<MEMO>" *> tagValueParser

-- | Parse until newline
untilNewLine :: forall m. Monad m => ParserT String m Unit
untilNewLine = void $ manyTill anyChar (char '\n')

-- | Parse a single statement transaction
statementTransactionParser :: forall m. Monad m => ParserT String m StatementTransaction
statementTransactionParser = do
  _ <- symbol "<STMTTRN>"
  _ <- symbol "<TRNTYPE>" <* untilNewLine
  dtPosted <- parseDtPosted
  amount <- transactionAmountParser
  fitId <- fitIdParser
  name <- nameParser
  memo <- memoParser
  -- Handle </STMTTRN> which might be on the same line as memo or on its own line
  _ <- ws *> string "</STMTTRN>" <* ws
  pure
    { posted: dtPosted
    , amount: amount
    , fitId: String.trim fitId
    , name: String.trim name
    , memo: String.trim memo
    }

-- | Main OFX parser
ofxParser :: forall m. Monad m => ParserT String m OfxBatch
ofxParser = do
  _ <- manyTill anyChar (string "<OFX>")
  _ <- manyTill anyChar (lookAhead (string "<ACCTID>"))
  accountNumber <- symbol "<ACCTID>" *> tagValueParser
  _ <- manyTill anyChar (lookAhead (string "<STMTTRN>"))
  transactions <- many statementTransactionParser
  _ <- manyTill anyChar (string "</OFX>")
  pure { accountNumber: accountNumber, transactions: Array.fromFoldable transactions }

-- | Parse an OFX file
parseOfx :: String -> Either String OfxBatch
parseOfx input = case runParser input ofxParser of
  Left err -> Left $ show err
  Right result -> Right result