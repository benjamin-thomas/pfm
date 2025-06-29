{-# LANGUAGE OverloadedStrings #-}

import Data.Decimal (DecimalRaw (Decimal))
import Data.Time
import Hello qualified
import OfxParser (TimeStamp (FullDate, ShortDate))
import OfxParser qualified
import Test.Hspec
import Text.Megaparsec qualified as P

{- FOURMOLU_DISABLE -}
{-

To run the tests, you have 2 options:

1) Use the REPL

$ cabal repl pfm-haskell-twain-test
ghci> :x :main

2) Use ghcid

$ ghcid -c "cabal repl pfm-haskell-twain-test" -T :main --reload src/ -W
$ ghcid -c "cabal repl test:test-dev" -T ":main" -W

 -}
{- FOURMOLU_ENABLE -}

helloSpec :: Spec
helloSpec = describe "Hello" $ do
    it "should return 'Hello, World!'" $ do
        Hello.world `shouldBe` "Hello, World!"

-- | `shouldParse` executes a parser, and returns a pretty error if it fails
shouldParse :: (P.VisualStream s, P.TraversableStream s, P.ShowErrorComponent e, Show a, Eq a) => P.Parsec e s a -> s -> a -> IO ()
shouldParse parser input expected =
    let srcFileName = ""
     in case P.parse parser srcFileName input of
            Left err -> fail $ P.errorBundlePretty err
            Right val -> val `shouldBe` expected

datePostedSpec :: Spec
datePostedSpec =
    describe "DTPOSTED" $ do
        it "parses a short date" $ do
            shouldBe
                (P.parse OfxParser.shortDateParser "" "20250617")
                (Right (fromGregorian 2025 6 17))
        it "parses a full date" $ do
            shouldBe
                (P.parse OfxParser.fullDateParser "" "20120103120000.123")
                ( Right
                    ( UTCTime
                        ( fromGregorian 2012 1 3
                        )
                        43200.123
                    )
                )
        it "parses a DTPOSTED tag (short date)" $ do
            shouldBe
                ( P.parse
                    OfxParser.parseDtPosted
                    ""
                    "<DTPOSTED>20250617\n<OTHER"
                )
                (Right (ShortDate (fromGregorian 2025 6 17)))

        it "parses a DTPOSTED tag (full date)" $ do
            let parser = do
                    date <- OfxParser.parseDtPosted
                    rest <- P.getInput
                    return (date, rest)
            shouldBe
                (P.parse parser "" "<DTPOSTED>20120103120000.123\n<OTHER")
                ( Right
                    ( FullDate (UTCTime (fromGregorian 2012 1 3) 43200.123)
                    , "\n<OTHER"
                    )
                )

transactionAmountSpec :: Spec
transactionAmountSpec = describe "TRNAMT" $ do
    it "parses a negative amount" $ do
        shouldParse
            OfxParser.transactionAmountParser
            "<TRNAMT>-49.95"
            (Decimal 2 (-4995))
    it "parses a positive amount" $ do
        shouldParse
            OfxParser.transactionAmountParser
            "<TRNAMT>99.99"
            (Decimal 2 9999)

ofxParserSpec :: Spec
ofxParserSpec = describe "OfxParser" $ do
    datePostedSpec
    transactionAmountSpec

main :: IO ()
main = hspec $ do
    helloSpec
    ofxParserSpec