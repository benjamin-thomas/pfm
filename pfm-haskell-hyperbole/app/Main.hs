module Main (main) where

import Data.ByteString.Lazy qualified as BSL
import Data.String.Interpolate (i)
import Data.Text (Text)

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
    { transactionDate :: Text
    , transactionDescr :: Text
    , transactionFrom :: Account
    , transactionTo :: Account
    , transactionAmount :: Double
    }
    deriving (Show, Eq, Generic)

instance ToJSON Transaction
instance FromJSON Transaction

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

-- Main Application
main :: IO ()
main = do
    mStr <- SE.lookupEnv "PORT"
    let port = fromMaybe 4321 $ readMaybe =<< mStr
    let staticMiddleware = staticPolicy (addBase "assets")
    putStrLn $ "Running on port: " <> show port
    run port $ staticMiddleware (app port)

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

app :: Int -> Application
app port = do
    liveApp
        (document port "PFM - Haskell/Hyperbole")
        (runPage page)

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

    update _ = pure pfmView

page :: Eff es (Page '[PfmView])
page = pure $ col id $ do
    hyper MkPfmView pfmView

pfmView :: View PfmView ()
pfmView = do
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
                    viewBalances

            div (extClass "section") $ do
                div (extClass "transaction-list") $ do
                    div (extClass "transaction-list__header") $ do
                        h3 id "Transactions"
                        button
                            AddTransaction
                            (extClass "button button--primary")
                            "Add Transaction"
                    div (extClass "transaction-list__body") $ do
                        viewTransactions

viewBalances :: View PfmView ()
viewBalances = do
    -- Fake balances for now
    balanceCard $ BalanceCardData 
        { balanceCardCategory = "Assets"
        , balanceCardAccount = "Checking account"
        , balanceCardAmount = "1.234,56 €"
        }
    
    balanceCard $ BalanceCardData 
        { balanceCardCategory = "Assets"
        , balanceCardAccount = "Savings account"
        , balanceCardAmount = "5.678,90 €"
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

viewTransactions :: View PfmView ()
viewTransactions = do
    -- Fake transactions for now
    transactionItem $ TransactionItemData
        { transactionItemDescription = "Salary"
        , transactionItemAccounts = "EmployerABC → Checking account"
        , transactionItemDate = "2025-04-15"
        , transactionItemAmount = "+3.000,00 €"
        , transactionItemIsPositive = True
        , transactionItemBalanceMovement = BalanceMovementData
            { balanceMovementBefore = "0,00 €"
            , balanceMovementAfter = "3.000,00 €"
            }
        }
        
    transactionItem $ TransactionItemData
        { transactionItemDescription = "Groceries"
        , transactionItemAccounts = "Checking account → Tesco"
        , transactionItemDate = "2025-04-18"
        , transactionItemAmount = "-75,50 €"
        , transactionItemIsPositive = False
        , transactionItemBalanceMovement = BalanceMovementData
            { balanceMovementBefore = "3.000,00 €"
            , balanceMovementAfter = "2.924,50 €"
            }
        }

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
                let amountClass = if data_.transactionItemIsPositive 
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
