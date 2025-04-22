module Main (main) where

import Control.Monad (forM_)
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

====== TO DEBUG ======
import Debug.Pretty.Simple (pTrace, pTraceShowWith)

dbg msg x = pTrace (msg <> ": " <> show x) x
======================

-}

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

instance ToJSON Transaction where
    toJSON t =
        object
            [ "transactionDate" .= transactionDate t
            , "transactionDescr" .= transactionDescr t
            , "transactionFrom" .= transactionFrom t
            , "transactionTo" .= transactionTo t
            , "transactionAmount" .= show (transactionAmount t)
            ]

instance FromJSON Transaction where
    parseJSON = withObject "Transaction" $ \v -> do
        date <- v .: "transactionDate"
        descr <- v .: "transactionDescr"
        from <- v .: "transactionFrom"
        to <- v .: "transactionTo"
        amountStr <- v .: "transactionAmount"
        case readMaybe amountStr of
            Just amount ->
                pure $
                    Transaction
                        { transactionDate = date
                        , transactionDescr = descr
                        , transactionFrom = from
                        , transactionTo = to
                        , transactionAmount = amount
                        }
            Nothing -> fail $ "Could not parse Decimal from: " ++ amountStr

data AppState = AppState
    { stateBook :: Map Int Transaction
    }
    deriving (Show, Eq, Generic)

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

allCategories :: [Category]
allCategories =
    [ categoryAssets
    , categoryExpenses
    , categoryIncome
    ]

allAccounts :: [Account]
allAccounts =
    [ checkingAccount
    , savingsAccount
    , spar
    , amazon
    , employerABC
    , openingBalance
    ]

accountsByCategory :: Category -> [Account]
accountsByCategory category = filter (\acc -> acc.accountCategory == category) allAccounts

assetsAccounts :: [Account]
assetsAccounts = accountsByCategory categoryAssets

expensesAccounts :: [Account]
expensesAccounts = accountsByCategory categoryExpenses

incomeAccounts :: [Account]
incomeAccounts = accountsByCategory categoryIncome

dec2 :: Double -> Decimal
dec2 = realFracToDecimal 2

initialState :: IO AppState
initialState = do
    now <- getCurrentTime
    let
        oneDay :: NominalDiffTime
        oneDay = 24 * 60 * 60

        refDate :: UTCTime
        refDate = addUTCTime (-7 * oneDay) now

        onDay :: Integer -> UTCTime
        onDay offset' = addUTCTime (fromIntegral offset' * oneDay) refDate
     in
        pure $
            AppState
                { stateBook =
                    Map.fromList
                        [
                            ( 1
                            , Transaction
                                { transactionDate = onDay 0
                                , transactionDescr = "Opening balance"
                                , transactionFrom = openingBalance
                                , transactionTo = checkingAccount
                                , transactionAmount = dec2 1000
                                }
                            )
                        ,
                            ( 2
                            , Transaction
                                { transactionDate = onDay 1
                                , transactionDescr = "Groceries"
                                , transactionFrom = checkingAccount
                                , transactionTo = spar
                                , transactionAmount = dec2 9.99
                                }
                            )
                        ,
                            ( 3
                            , Transaction
                                { transactionDate = onDay 2
                                , transactionDescr = "Book purchase"
                                , transactionFrom = checkingAccount
                                , transactionTo = amazon
                                , transactionAmount = dec2 54.99
                                }
                            )
                        ,
                            ( 4
                            , Transaction
                                { transactionDate = onDay 3
                                , transactionDescr = "Groceries, again"
                                , transactionFrom = checkingAccount
                                , transactionTo = spar
                                , transactionAmount = dec2 37.42
                                }
                            )
                        ,
                            ( 5
                            , Transaction
                                { transactionDate = onDay 4
                                , transactionDescr = "Salary"
                                , transactionFrom = employerABC
                                , transactionTo = checkingAccount
                                , transactionAmount = dec2 100
                                }
                            )
                        ]
                }

formatAmountFR :: Bool -> Decimal -> Text
formatAmountFR posSign amount =
    sign <> amount' <> " €"
  where
    sign = if amount >= 0 && posSign then "+" else ""
    amount' =
        T.pack $ insertSeparator ' ' [if x == '.' then ',' else x | x <- show amount]

{-

>>> insertSeparator '.' ""
""

>>> insertSeparator '.' "1"
"1"

>>> insertSeparator '.' "1234"
"1.234"

>>> insertSeparator '.' "123456789"
"123.456.789"

 -}

insertSeparator :: Char -> String -> String
insertSeparator sep str =
    snd $ foldr step (0, []) str
  where
    len :: Int
    len = length str

    step :: Char -> (Int, String) -> (Int, String)
    step c (n, acc) =
        if n /= len - 1 && n `mod` 3 == 2
            then (n + 1, sep : c : acc)
            else (n + 1, c : acc)

formatDate :: UTCTime -> Text
formatDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

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
page state = pure $ col id $ do
    hyper MkPfmView (pfmView state)

pfmView :: AppState -> View PfmView ()
pfmView state = do
    let balances = calculateBalances state
    div (extClass "app") do
        div (extClass "container") do
            h1 (att "style" "margin-bottom:0") "PFM"
            h4 (att "style" "margin-top:3px;margin-bottom:8px") "In Haskell/Hyperbole"

            div (extClass "section") do
                h2 (extClass "section-title") "Balances"
                div (extClass "balances") do
                    viewBalances balances

            div (extClass "section") do
                div (extClass "transaction-list") do
                    div (extClass "transaction-list__header") do
                        h3 (att "id" "transactions") "Transactions"
                        button
                            AddTransaction
                            (extClass "button button--primary")
                            "Add Transaction"
                    div (extClass "transaction-list__body") do
                        viewTransactions state

calculateBalances :: AppState -> Map Account Decimal
calculateBalances AppState{stateBook} =
    foldr updateBalances Map.empty (Map.elems stateBook)
  where
    updateBalances tx acc =
        let fromAdjustment = Map.insertWith (+) tx.transactionFrom (-tx.transactionAmount) acc
            toAdjustment = Map.insertWith (+) tx.transactionTo tx.transactionAmount fromAdjustment
         in toAdjustment

viewBalances :: Map Account Decimal -> View PfmView ()
viewBalances balances = do
    forM_ assetsAccounts $ \account -> do
        let amount = Map.findWithDefault (dec2 0) account balances

        balanceCard account amount

balanceCard :: Account -> Decimal -> View PfmView ()
balanceCard account amount =
    div (extClass "balance-card") do
        div (extClass "balance-card__category") do
            text account.accountCategory.categoryName
        div (extClass "balance-card__account") do
            text account.accountName
        div (extClass "balance-card__amount") do
            text $ formatAmountFR True amount

viewTransactions :: AppState -> View PfmView ()
viewTransactions state = do
    let transactions = Map.toList state.stateBook
    let sortedTransactions = sortBy (\(_, a) (_, b) -> compare b.transactionDate a.transactionDate) transactions

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
                , transactionItemAmount = formatAmountFR True (if isPositive then transaction.transactionAmount else -transaction.transactionAmount)
                , transactionItemIsPositive = isPositive
                , transactionItemBalanceMovement =
                    BalanceMovementData
                        { balanceMovementBefore = formatAmountFR False balanceBefore
                        , balanceMovementAfter = formatAmountFR False balanceAfter
                        }
                }

calculateBalanceMovements :: [(Int, Transaction)] -> [(Int, Transaction, Decimal, Decimal)]
calculateBalanceMovements transactions =
    let
        sortedTransactions = sortBy (\(_, a) (_, b) -> compare b.transactionDate a.transactionDate) transactions

        calcBalances [] _ acc = acc
        calcBalances ((tid, tx) : txs) currentBalance acc =
            let adjustment =
                    if tx.transactionTo == checkingAccount
                        then tx.transactionAmount
                        else
                            if tx.transactionFrom == checkingAccount
                                then -tx.transactionAmount
                                else dec2 0
                newBalance = currentBalance - adjustment
                entry = (tid, tx, newBalance, currentBalance)
             in calcBalances txs newBalance (entry : acc)
     in
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
                    (dec2 0)
                    transactions
         in calcBalances sortedTransactions initialBalance []

transactionItem :: TransactionItemData -> View PfmView ()
transactionItem data_ =
    div (extClass "transaction-item") do
        div (extClass "transaction-item__row") do
            div (extClass "transaction-item__main-content") do
                div (extClass "transaction-item__details") do
                    div (extClass "transaction-item__description") do
                        text data_.transactionItemDescription
                    div (extClass "transaction-item__accounts") do
                        text data_.transactionItemAccounts
                div (extClass "transaction-item__date") do
                    text data_.transactionItemDate
                let amountClass =
                        if data_.transactionItemIsPositive
                            then "transaction-item__amount transaction-item__amount--positive"
                            else "transaction-item__amount transaction-item__amount--negative"
                div (extClass amountClass) do
                    text data_.transactionItemAmount
            transactionBalanceMovement data_.transactionItemBalanceMovement

transactionBalanceMovement :: BalanceMovementData -> View PfmView ()
transactionBalanceMovement data_ =
    div (extClass "transaction-item__balance-column") do
        div (extClass "transaction-item__balance-movement") do
            span (extClass "balance-before") (text data_.balanceMovementBefore)
            span (extClass "arrow-icon") (text " → ")
            span (extClass "balance-after") (text data_.balanceMovementAfter)

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
