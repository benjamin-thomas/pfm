module Test.OfxParser.Spec where

import Prelude

import Data.Array ((!!), length)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Partial.Unsafe (unsafePartial)
import Server.OfxParser (TimeStamp(..), parseOfx)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

-- | Helper to create a date
mkDate :: Int -> Int -> Int -> Date
mkDate year month day = unsafePartial $ fromJust do
  y <- toEnum year
  m <- toEnum month
  d <- toEnum day
  pure $ canonicalDate y m d

-- | Helper to create a datetime
mkDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> DateTime
mkDateTime year month day hour minute second millisecond = unsafePartial $ fromJust do
  y <- toEnum year
  mo <- toEnum month
  d <- toEnum day
  h <- toEnum hour
  mi <- toEnum minute
  s <- toEnum second
  ms <- toEnum millisecond
  pure $ DateTime (canonicalDate y mo d) (Time h mi s ms)

-- | Helper to create a decimal from a string
mkDecimal :: String -> Decimal
mkDecimal str = unsafePartial $ fromJust $ Decimal.fromString str

spec :: Spec Unit
spec = do
  describe "OfxParser" do
    describe "parseOfx" do
      it "should parse a simple OFX file" do
        let
          simpleOfx =
            """<OFX>
<ACCTID>12345678
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240101
<TRNAMT>-49.95
<FITID>2024010101
<NAME>Test Transaction
<MEMO>Test Memo
</STMTTRN>
</OFX>"""

        case parseOfx simpleOfx of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            batch.accountNumber `shouldEqual` "12345678"
            batch.transactions `shouldSatisfy` \txs -> length txs == 1

            case batch.transactions !! 0 of
              Nothing -> fail "No transactions found"
              Just txn -> do
                txn.posted `shouldEqual` ShortDate (mkDate 2024 1 1)
                txn.amount `shouldEqual` mkDecimal "-49.95"
                txn.fitId `shouldEqual` "2024010101"
                txn.name `shouldEqual` "Test Transaction"
                txn.memo `shouldEqual` "Test Memo"

      it "should parse multiple transactions" do
        let
          multiOfx =
            """<OFX>
<ACCTID>87654321
<STMTTRN>
<TRNTYPE>CREDIT
<DTPOSTED>20240102
<TRNAMT>+100.00
<FITID>2024010201
<NAME>Deposit
<MEMO>Salary
</STMTTRN>
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20240103
<TRNAMT>-25.50
<FITID>2024010301
<NAME>Grocery Store
<MEMO>Food
</STMTTRN>
</OFX>"""

        case parseOfx multiOfx of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            batch.accountNumber `shouldEqual` "87654321"
            batch.transactions `shouldSatisfy` \txs -> length txs == 2

      it "should parse full datetime format" do
        let
          fullDateOfx =
            """<OFX>
<ACCTID>11111111
<STMTTRN>
<TRNTYPE>DEBIT
<DTPOSTED>20120103120000.123
<TRNAMT>-49.95
<FITID>201201031
<NAME>PLANET BEACH AL001
<MEMO>RECUR DEBIT CRD PMT0
</STMTTRN>
</OFX>"""

        case parseOfx fullDateOfx of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            case batch.transactions !! 0 of
              Nothing -> fail "No transactions found"
              Just txn -> do
                txn.posted `shouldEqual` FullDate (mkDateTime 2012 1 3 12 0 0 123)

      it "should trim whitespace from text fields" do
        let
          spacedOfx =
            """<OFX>
<ACCTID>22222222
<STMTTRN>
<TRNTYPE>OTHER
<DTPOSTED>20240104
<TRNAMT>-10.00
<FITID>2024010401
<NAME>  Spaced Name  
<MEMO>  Spaced Memo  
</STMTTRN>
</OFX>"""

        case parseOfx spacedOfx of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            case batch.transactions !! 0 of
              Nothing -> fail "No transactions found"
              Just txn -> do
                txn.name `shouldEqual` "Spaced Name"
                txn.memo `shouldEqual` "Spaced Memo"

      it "should parse the fixture file" do
        content <- FS.readTextFile UTF8 "test/OfxParser/fixture.ofx"

        case parseOfx content of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            batch.accountNumber `shouldEqual` "04112816394"
            batch.transactions `shouldSatisfy` \txs -> length txs == 10

            -- Check first transaction
            case batch.transactions !! 0 of
              Nothing -> fail "No first transaction found"
              Just txn -> do
                txn.posted `shouldEqual` ShortDate (mkDate 2016 4 13)
                txn.amount `shouldEqual` mkDecimal "4000.00"
                txn.fitId `shouldEqual` "9947030000068"
                txn.name `shouldEqual` "A client"
                txn.memo `shouldEqual` "Transfer in your favor"

            -- Check a negative transaction
            case batch.transactions !! 1 of
              Nothing -> fail "No second transaction found"
              Just txn -> do
                txn.posted `shouldEqual` ShortDate (mkDate 2016 4 11)
                txn.amount `shouldEqual` mkDecimal "-31.20"
                txn.fitId `shouldEqual` "9944290089129"
                txn.name `shouldEqual` "A Book publisher 10/04"
                txn.memo `shouldEqual` "Payment by card"

      it "should handle amounts without explicit sign" do
        let
          noSignOfx =
            """<OFX>
<ACCTID>33333333
<STMTTRN>
<TRNTYPE>CREDIT
<DTPOSTED>20240105
<TRNAMT>50.00
<FITID>2024010501
<NAME>Unsigned Amount
<MEMO>No sign
</STMTTRN>
</OFX>"""

        case parseOfx noSignOfx of
          Left err -> fail $ "Parser failed: " <> err
          Right batch -> do
            case batch.transactions !! 0 of
              Nothing -> fail "No transactions found"
              Just txn -> do
                txn.amount `shouldEqual` mkDecimal "50.00"