module Main (main) where

import Control.Monad (forM_, when)
import Data.ByteString.Lazy qualified as BSL
import Data.Decimal (Decimal, realFracToDecimal)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time

import Network.Wai.Middleware.Static (addBase, staticPolicy)

import Web.Hyperbole hiding (input)
import Web.View.Style (extClass)
import Prelude hiding (div, span)

import Data.Aeson
import Data.Maybe (fromMaybe)
import DevReload (devReloadPageJs)
import System.Environment qualified as SE
import Text.Read (readMaybe)

{-
PORT=1234 ghcid -c 'cabal repl pfm-haskell-hyperbole' -T :main --warnings --reload=./assets/css/main.css
-}

-- Custom JSON instances for Decimal
instance ToJSON Decimal where
    toJSON d = toJSON (show d)

instance FromJSON Decimal where
    parseJSON v = do
        s <- parseJSON v
        case readMaybe s of
            Just d -> pure d
            Nothing -> fail $ "Could not parse Decimal from: " ++ s

-- Domain Types
data Category = Category
    { categoryName :: Text
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Category
instance FromJSON Category

data Account = Account
    { accountName :: Text
    , accountCategory :: Category
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON Account
instance FromJSON Account

data Transaction = Transaction
    { transactionDate :: UTCTime
    , transactionDescr :: Text
    , transactionFrom :: Account
    , transactionTo :: Account
    , transactionAmount :: Decimal
    }
    deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

-- Application State
data AppState = AppState
    { stateBook :: Map Int Transaction
    }
    deriving (Show, Eq, Generic)

-- UI Component Types
data BalanceCardData = BalanceCardData
    { balanceCardCategory :: Text
    , balanceCardAccount :: Text
    , balanceCardAmount :: Text
    }
    deriving (Show, Eq, Generic)

data BalanceMovementData = BalanceMovementData
    { balanceMovementBefore :: Text
    , balanceMovementAfter :: Text
    }
    deriving (Show, Eq, Generic)

data TransactionItemData = TransactionItemData
    { transactionItemDescription :: Text
    , transactionItemAccounts :: Text
    , transactionItemDate :: Text
    , transactionItemAmount :: Text
    , transactionItemIsPositive :: Bool
    , transactionItemBalanceMovement :: BalanceMovementData
    }
    deriving (Show, Eq, Generic)

-- Sample Data
categoryAssets :: Category
categoryAssets = Category{categoryName = "Assets"}

categoryExpenses :: Category
categoryExpenses = Category{categoryName = "Expenses"}

categoryIncome :: Category
categoryIncome = Category{categoryName = "Income"}

openingBalance :: Account
openingBalance = Account{accountName = "Opening Balance", accountCategory = categoryIncome}

checkingAccount :: Account
checkingAccount = Account{accountName = "Checking Account", accountCategory = categoryAssets}

savingsAccount :: Account
savingsAccount = Account{accountName = "Savings Account", accountCategory = categoryAssets}

spar :: Account
spar = Account{accountName = "Spar", accountCategory = categoryExpenses}

amazon :: Account
amazon = Account{accountName = "Amazon", accountCategory = categoryExpenses}

employerABC :: Account
employerABC = Account{accountName = "Employer ABC", accountCategory = categoryIncome}

-- All categories in the system
allCategories :: [Category]
allCategories = [categoryAssets, categoryExpenses, categoryIncome]

-- All accounts in the system
allAccounts :: [Account]
allAccounts =
    [ checkingAccount
    , savingsAccount
    , spar
    , amazon
    , employerABC
    , openingBalance
    ]

-- Filter accounts by category
accountsByCategory :: Category -> [Account]
accountsByCategory category = filter (\acc -> acc.accountCategory == category) allAccounts

-- Assets accounts
assetsAccounts :: [Account]
assetsAccounts = accountsByCategory categoryAssets

-- Expenses accounts
expensesAccounts :: [Account]
expensesAccounts = accountsByCategory categoryExpenses

-- Income accounts
incomeAccounts :: [Account]
incomeAccounts = accountsByCategory categoryIncome

-- Helper function to create dates relative to today
onDay :: Integer -> IO UTCTime
onDay daysAgo = do
    now <- getCurrentTime
    let dayInSeconds = 24 * 60 * 60
    pure $ addUTCTime (fromIntegral $ -daysAgo * dayInSeconds) now

-- Helper function to create Decimal values
dec :: Double -> Decimal
dec = realFracToDecimal 2

-- Initial state
initialState :: IO AppState
initialState = do
    today <- getCurrentTime
    yesterday <- onDay 1
    twoDaysAgo <- onDay 2
    threeDaysAgo <- onDay 3
    fourDaysAgo <- onDay 4

    pure $
        AppState
            { stateBook =
                Map.fromList
                    [
                        ( 1
                        , Transaction
                            { transactionDate = today
                            , transactionDescr = "Opening balance"
                            , transactionFrom = openingBalance
                            , transactionTo = checkingAccount
                            , transactionAmount = dec 1000.00
                            }
                        )
                    ,
                        ( 2
                        , Transaction
                            { transactionDate = yesterday
                            , transactionDescr = "Groceries"
                            , transactionFrom = checkingAccount
                            , transactionTo = spar
                            , transactionAmount = dec 9.99
                            }
                        )
                    ,
                        ( 3
                        , Transaction
                            { transactionDate = twoDaysAgo
                            , transactionDescr = "Book purchase"
                            , transactionFrom = checkingAccount
                            , transactionTo = amazon
                            , transactionAmount = dec 54.99
                            }
                        )
                    ,
                        ( 4
                        , Transaction
                            { transactionDate = threeDaysAgo
                            , transactionDescr = "Groceries, again"
                            , transactionFrom = checkingAccount
                            , transactionTo = spar
                            , transactionAmount = dec 37.42
                            }
                        )
                    ,
                        ( 5
                        , Transaction
                            { transactionDate = fourDaysAgo
                            , transactionDescr = "Salary"
                            , transactionFrom = employerABC
                            , transactionTo = checkingAccount
                            , transactionAmount = dec 100.00
                            }
                        )
                    ]
            }

-- Helper functions for formatting
formatAmount :: Decimal -> Text
formatAmount amount =
    let sign = if amount >= 0 then "+" else ""
        -- Format with 2 decimal places and thousands separator
        amountStr = formatDecimal amount
     in sign <> amountStr <> " €"

-- Format decimal with thousands separator and 2 decimal places
formatDecimal :: Decimal -> Text
formatDecimal d =
    let
        -- Convert to string with 2 decimal places
        str = show d
        -- Split into parts before and after decimal point
        (intPart, decPart) = case T.splitOn "." (T.pack str) of
            [i, d] -> (i, d)
            [i] -> (i, "00")
            _ -> ("0", "00")
        -- Ensure 2 decimal places
        decPart' = T.take 2 (decPart <> "00")
        -- Add thousands separator to integer part
        intPart' = addThousandsSeparator intPart
     in
        intPart' <> "," <> decPart'

-- Add thousands separator to integer part
addThousandsSeparator :: Text -> Text
addThousandsSeparator t =
    let len = T.length t
        chunks = reverse [T.take 3 (T.drop i t) | i <- [0, 3 .. len - 1]]
     in if len <= 3
            then t
            else T.intercalate "." (reverse chunks)

formatDate :: UTCTime -> Text
formatDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

-- Main Application
main :: IO ()
main = do
    mStr <- SE.lookupEnv "PORT"
    let port = fromMaybe 4321 $ readMaybe =<< mStr
    let staticMiddleware = staticPolicy (addBase "assets")
    state <- initialState
    putStrLn $ "Running on port: " <> show port
    run port $ staticMiddleware (app port state)

document :: Int -> Text -> BSL.ByteString -> BSL.ByteString
document port title cnt =
    [i|
    <!DOCTYPE html>
    <html class="dark-theme">
      <head>
        <title>#{title}</title>
        <script type="text/javascript">#{scriptEmbed}</script>

        <link href="/css/main.css"
              rel="stylesheet">

        <script>
          #{devReloadPageJs port}
        </script>

      </head>
      <body>#{cnt}</body>
  </html>|]

app :: Int -> AppState -> Application
app port state = do
    liveApp
        (document port "PFM - Haskell/Hyperbole")
        (runPage $ page state)

-- Views
data PfmView = MkPfmView
    deriving
        ( Show
        , Read
        , Generic
        , ViewId
        )

instance HyperView PfmView es where
    data Action PfmView
        = AddTransaction
        | EditTransaction Int
        | NoOp
        deriving
            ( Show
            , Read
            , Generic
            , ViewAction
            )

    update _ = pure $ pfmView dummyState
      where
        dummyState = AppState{stateBook = Map.empty}

page :: AppState -> Eff es (Page '[PfmView])
page state = pure $ col attClass $ do
    hyper MkPfmView (pfmView state)
  where
    attClass = extClass "" -- Empty class, just to satisfy the type

pfmView :: AppState -> View PfmView ()
pfmView state = do
    div (extClass "app") $ do
        div (extClass "container") $ do
            div (extClass "section") $ do
                div (extClass "debug-info") $ do
                    text "Haskell/Hyperbole Version"

            h1 (att "style" "margin-bottom:0") "PFM"
            h4 (att "style" "margin-top:3px;margin-bottom:8px") "In Haskell/Hyperbole"

            div (extClass "section") $ do
                h2 (extClass "section-title") "Balances"
                div (extClass "balances") $ do
                    viewBalances state

            div (extClass "section") $ do
                div (extClass "transaction-list") $ do
                    div (extClass "transaction-list__header") $ do
                        h3 (att "id" "transactions") "Transactions"
                        button
                            AddTransaction
                            (extClass "button button--primary")
                            "Add Transaction"
                    div (extClass "transaction-list__body") $ do
                        viewTransactions state

-- Calculate balances based on transactions
calculateBalances :: AppState -> Map Account Decimal
calculateBalances AppState{stateBook} =
    foldr updateBalances Map.empty (Map.elems stateBook)
  where
    updateBalances tx acc =
        let fromAdjustment = Map.insertWith (+) tx.transactionFrom (-tx.transactionAmount) acc
            toAdjustment = Map.insertWith (+) tx.transactionTo tx.transactionAmount fromAdjustment
         in toAdjustment

viewBalances :: AppState -> View PfmView ()
viewBalances state = do
    let balances = calculateBalances state

    -- Display only asset accounts, like in the Elm version
    forM_ assetsAccounts $ \account -> do
        when (Map.member account balances) $ do
            let amount = Map.findWithDefault (dec 0) account balances

            balanceCard $
                BalanceCardData
                    { balanceCardCategory = account.accountCategory.categoryName
                    , balanceCardAccount = account.accountName
                    , balanceCardAmount = formatAmount amount
                    }

balanceCard :: BalanceCardData -> View PfmView ()
balanceCard data_ =
    div (extClass "balance-card") $ do
        div (extClass "balance-card__category") $ do
            text data_.balanceCardCategory
        div (extClass "balance-card__account") $ do
            text data_.balanceCardAccount
        div (extClass "balance-card__amount") $ do
            text data_.balanceCardAmount

viewTransactions :: AppState -> View PfmView ()
viewTransactions state = do
    let transactions = Map.toList state.stateBook
    let sortedTransactions = sortBy (\(_, a) (_, b) -> compare b.transactionDate a.transactionDate) transactions

    -- Filter transactions related to checking account
    let checkingTransactions =
            filter
                ( \(_, tx) ->
                    tx.transactionFrom == checkingAccount
                        || tx.transactionTo == checkingAccount
                )
                sortedTransactions

    let balances = calculateBalanceMovements checkingTransactions

    forM_ balances $ \(_, transaction, balanceBefore, balanceAfter) -> do
        let isPositive = transaction.transactionTo == checkingAccount
        let accountFlow =
                if isPositive
                    then transaction.transactionFrom.accountName <> " → " <> transaction.transactionTo.accountName
                    else transaction.transactionFrom.accountName <> " → " <> transaction.transactionTo.accountName

        transactionItem $
            TransactionItemData
                { transactionItemDescription = transaction.transactionDescr
                , transactionItemAccounts = accountFlow
                , transactionItemDate = formatDate transaction.transactionDate
                , transactionItemAmount = formatAmount (if isPositive then transaction.transactionAmount else -transaction.transactionAmount)
                , transactionItemIsPositive = isPositive
                , transactionItemBalanceMovement =
                    BalanceMovementData
                        { balanceMovementBefore = formatAmount balanceBefore
                        , balanceMovementAfter = formatAmount balanceAfter
                        }
                }

-- Calculate balance movements for each transaction
calculateBalanceMovements :: [(Int, Transaction)] -> [(Int, Transaction, Decimal, Decimal)]
calculateBalanceMovements transactions =
    let
        -- Start with the most recent transaction and work backwards
        sortedTransactions = sortBy (\(_, a) (_, b) -> compare b.transactionDate a.transactionDate) transactions

        -- Calculate running balance for checking account
        calcBalances [] _ acc = acc
        calcBalances ((tid, tx) : txs) currentBalance acc =
            let adjustment =
                    if tx.transactionTo == checkingAccount
                        then tx.transactionAmount
                        else
                            if tx.transactionFrom == checkingAccount
                                then -tx.transactionAmount
                                else dec 0
                newBalance = currentBalance - adjustment
                entry = (tid, tx, newBalance, currentBalance)
             in calcBalances txs newBalance (entry : acc)
     in
        -- Start with balance after all transactions
        let initialBalance =
                foldr
                    ( \(_, tx) bal ->
                        if tx.transactionTo == checkingAccount
                            then bal + tx.transactionAmount
                            else
                                if tx.transactionFrom == checkingAccount
                                    then bal - tx.transactionAmount
                                    else bal
                    )
                    (dec 0)
                    transactions
         in calcBalances sortedTransactions initialBalance []

transactionItem :: TransactionItemData -> View PfmView ()
transactionItem data_ =
    div (extClass "transaction-item") $ do
        div (extClass "transaction-item__row") $ do
            div (extClass "transaction-item__main-content") $ do
                div (extClass "transaction-item__details") $ do
                    div (extClass "transaction-item__description") $ do
                        text data_.transactionItemDescription
                    div (extClass "transaction-item__accounts") $ do
                        text data_.transactionItemAccounts
                div (extClass "transaction-item__date") $ do
                    text data_.transactionItemDate
                let amountClass =
                        if data_.transactionItemIsPositive
                            then "transaction-item__amount transaction-item__amount--positive"
                            else "transaction-item__amount transaction-item__amount--negative"
                div (extClass amountClass) $ do
                    text data_.transactionItemAmount
            transactionBalanceMovement data_.transactionItemBalanceMovement

transactionBalanceMovement :: BalanceMovementData -> View PfmView ()
transactionBalanceMovement data_ =
    div (extClass "transaction-item__balance-column") $ do
        div (extClass "transaction-item__balance-movement") $ do
            span (extClass "balance-before") $ text data_.balanceMovementBefore
            span (extClass "arrow-icon") $ text " → "
            span (extClass "balance-after") $ text data_.balanceMovementAfter

-- HTML Helpers
h1 :: Mod c -> View c () -> View c ()
h1 = tag "h1"

h2 :: Mod c -> View c () -> View c ()
h2 = tag "h2"

h3 :: Mod c -> View c () -> View c ()
h3 = tag "h3"

h4 :: Mod c -> View c () -> View c ()
h4 = tag "h4"

div :: Mod c -> View c () -> View c ()
div = tag "div"

span :: Mod c -> View c () -> View c ()
span = tag "span"

-- Pure functions for testing
calculateAccountBalance :: Map Int Transaction -> Account -> Decimal
calculateAccountBalance transactions account =
    foldr updateBalance (dec 0) (Map.elems transactions)
  where
    updateBalance tx acc
        | tx.transactionTo == account = acc + tx.transactionAmount
        | tx.transactionFrom == account = acc - tx.transactionAmount
        | otherwise = acc

-- Test function for formatting
testFormatAmount :: Decimal -> Text
testFormatAmount = formatAmount
