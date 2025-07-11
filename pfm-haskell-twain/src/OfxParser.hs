{-# LANGUAGE OverloadedStrings #-}

module OfxParser where

import Data.Decimal
  ( Decimal
  , DecimalRaw (Decimal)
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( Day
  , UTCTime (UTCTime)
  , fromGregorianValid
  )
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as CL

type Parser = Parsec Void Text

-- cspell:disable
{-

<STMTTRN>
  <TRNTYPE>DEBIT
  <DTPOSTED>20120103120000.000
  <TRNAMT>-49.95
  <FITID>201201031
  <NAME>PLANET BEACH AL001
  <MEMO>RECUR DEBIT CRD PMT0
</STMTTRN>

 -}
-- cspell:enable

{-

Source: https://financialdataexchange.org/common/Uploaded%20files/OFX%20files/OFX%20Banking%20Specification%20v2.3.pdf

3.2.3 Financial Institution Transaction ID <FITID>

If a client performs the same type of request
within the same scope at two different FIs, clients will need to use FI + <ACCTID> + <FITID> as a
globally unique key in a client database. That is, the <FITID> value must be unique within the account and
Financial Institution (independent of the service provider).

 -}

data StatementTransaction
  = MkStatementTransaction
  { stPosted :: TimeStamp
  , stAmount :: Decimal
  , stFitId :: Text -- FinancialInstitutionTransactionId
  , stName :: Text
  , stMemo :: Text
  }
  deriving (Show, Eq)

tagValueParser :: Parser Text
tagValueParser =
  takeWhile1P Nothing (\c -> c /= '\n' && c /= '>')

shortDateParser :: Parser Day
shortDateParser = do
  y <- read <$> count 4 digitChar
  m <- read <$> count 2 digitChar
  d <- read <$> count 2 digitChar

  maybe (fail "Invalid date") pure $ fromGregorianValid y m d

fullDateParser :: Parser UTCTime
fullDateParser = do
  y <- read <$> count 4 digitChar
  m <- read <$> count 2 digitChar
  d <- read <$> count 2 digitChar
  h <- read <$> count 2 digitChar :: Parser Int
  m' <- read <$> count 2 digitChar
  s <- read <$> count 2 digitChar
  _ <- char '.'
  ms <- read <$> count 3 digitChar :: Parser Int

  let secs =
        sum
          [ fromIntegral (h * 3600 + m' * 60 + s)
          , fromIntegral ms / 1000
          ]

  case fromGregorianValid y m d of
    Just day -> pure $ UTCTime day secs
    Nothing -> fail "Invalid date"

data TimeStamp
  = FullDate UTCTime
  | ShortDate Day
  deriving (Show, Eq)

parseDtPosted :: Parser TimeStamp
parseDtPosted = do
  _ <- symbol "<DTPOSTED>"
  try (FullDate <$> fullDateParser) <|> (ShortDate <$> shortDateParser)

transactionAmountParser :: Parser Decimal
transactionAmountParser =
  let decimalPlaces = 2
      scale = 10 ^ decimalPlaces
   in do
        _ <- symbol "<TRNAMT>"
        isNeg <- (True <$ char '-') <|> (False <$ char '+') <|> pure False
        intPart <- read <$> many digitChar
        _ <- char '.'
        fracPart <- read <$> many digitChar
        let cents = intPart * scale + fracPart
        pure $ Decimal decimalPlaces (if isNeg then -cents else cents)

fitIdParser :: Parser Text
fitIdParser =
  symbol "<FITID>" *> tagValueParser

nameParser :: Parser Text
nameParser =
  symbol "<NAME>" *> tagValueParser

memoParser :: Parser Text
memoParser =
  symbol "<MEMO>" *> tagValueParser

ws :: Parser ()
ws = CL.space space1 empty empty

symbol :: Text -> Parser Text
symbol txt = ws *> string txt <* ws

untilNewLine :: Parser [Token Text]
untilNewLine =
  manyTill anySingle "\n"

ignoreWs :: Parser a -> Parser a
ignoreWs p = ws *> p <* ws

statementTransactionParser :: Parser StatementTransaction
statementTransactionParser = do
  _ <- symbol "<STMTTRN>"
  _ <- symbol "<TRNTYPE>" <* untilNewLine
  dtPosted <- parseDtPosted
  amount <- transactionAmountParser
  fitId <- fitIdParser
  name <- nameParser
  memo <- memoParser
  _ <- symbol "</STMTTRN>"
  pure $
    MkStatementTransaction
      { stPosted = dtPosted
      , stAmount = amount
      , stFitId = T.strip fitId
      , stName = T.strip name
      , stMemo = T.strip memo
      }

data OfxBatch = MkOfxBatch
  { ofxBatchAccountNumber :: Text
  , ofxBatchTransactions :: [StatementTransaction]
  }

ofxParser :: Parser OfxBatch
ofxParser = do
  _ <- manyTill anySingle "<OFX>" -- quick and dirty
  _ <- manyTill anySingle (lookAhead (string "<ACCTID>"))
  accountNumber <- symbol "<ACCTID>" *> tagValueParser
  _ <- manyTill anySingle (lookAhead (string "<STMTTRN>"))
  transactions <- many statementTransactionParser <* manyTill anySingle "</OFX>"
  pure $ MkOfxBatch accountNumber transactions