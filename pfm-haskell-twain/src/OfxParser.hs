{-# LANGUAGE OverloadedStrings #-}

module OfxParser where

import Data.Decimal
  ( Decimal
  , DecimalRaw (Decimal)
  )
import Data.Text (Text)
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
  _ <- string "<DTPOSTED>"
  try (FullDate <$> fullDateParser) <|> (ShortDate <$> shortDateParser)

transactionAmountParser :: Parser Decimal
transactionAmountParser =
  let decimalPlaces = 2
      scale = 10 ^ decimalPlaces
   in do
        _ <- string "<TRNAMT>"
        isNeg <- (True <$ char '-') <|> pure False
        intPart <- read <$> many digitChar
        _ <- char '.'
        fracPart <- read <$> many digitChar
        let cents = intPart * scale + fracPart
        pure $ Decimal decimalPlaces (if isNeg then -cents else cents)

fitIdParser :: Parser Text
fitIdParser =
  string "<FITID>" *> tagValueParser

nameParser :: Parser Text
nameParser =
  string "<NAME>" *> tagValueParser

memoParser :: Parser Text
memoParser =
  string "<MEMO>" *> tagValueParser

ws :: Parser ()
ws = CL.space space1 empty empty

statementTransactionParser :: Parser StatementTransaction
statementTransactionParser = do
  _ <- ws *> string "<STMTTRN>" <* ws
  _ <- ws *> string "<TRNTYPE>" <* manyTill anySingle "\n"
  dtPosted <- ws *> parseDtPosted <* ws
  amount <- ws *> transactionAmountParser <* ws
  fitId <- ws *> fitIdParser <* ws
  name <- ws *> nameParser <* ws
  memo <- ws *> memoParser <* ws
  _ <- ws *> string "</STMTTRN>" <* ws
  pure $
    MkStatementTransaction
      { stPosted = dtPosted
      , stAmount = amount
      , stFitId = fitId
      , stName = name
      , stMemo = memo
      }
