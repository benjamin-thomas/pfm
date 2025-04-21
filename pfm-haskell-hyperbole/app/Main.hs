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
    div (extClass "balance-card") $ do
        div (extClass "balance-card__category") $ do
            text "Assets"
        div (extClass "balance-card__account") $ do
            text "Checking account"
        div (extClass "balance-card__amount") $ do
            text "1.234,56 €"

    div (extClass "balance-card") $ do
        div (extClass "balance-card__category") $ do
            text "Assets"
        div (extClass "balance-card__account") $ do
            text "Savings account"
        div (extClass "balance-card__amount") $ do
            text "5.678,90 €"

viewTransactions :: View PfmView ()
viewTransactions = do
    -- Fake transactions for now
    div (extClass "transaction-item") $ do
        div (extClass "transaction-item__row") $ do
            div (extClass "transaction-item__main-content") $ do
                div (extClass "transaction-item__details") $ do
                    div (extClass "transaction-item__description") $ do
                        text "Salary"
                    div (extClass "transaction-item__accounts") $ do
                        text "EmployerABC → Checking account"
                div (extClass "transaction-item__date") $ do
                    text "2025-04-15"
                div (extClass "transaction-item__amount transaction-item__amount--positive") $ do
                    text "+3.000,00 €"
            div (extClass "transaction-item__balance-column") $ do
                div (extClass "transaction-item__balance-movement") $ do
                    span (extClass "balance-before") $ text "0,00 €"
                    span (extClass "arrow-icon") $ text " → "
                    span (extClass "balance-after") $ text "3.000,00 €"

    div (extClass "transaction-item") $ do
        div (extClass "transaction-item__row") $ do
            div (extClass "transaction-item__main-content") $ do
                div (extClass "transaction-item__details") $ do
                    div (extClass "transaction-item__description") $ do
                        text "Groceries"
                    div (extClass "transaction-item__accounts") $ do
                        text "Checking account → Tesco"
                div (extClass "transaction-item__date") $ do
                    text "2025-04-18"
                div (extClass "transaction-item__amount transaction-item__amount--negative") $ do
                    text "-75,50 €"
            div (extClass "transaction-item__balance-column") $ do
                div (extClass "transaction-item__balance-movement") $ do
                    span (extClass "balance-before") $ text "3.000,00 €"
                    span (extClass "arrow-icon") $ text " → "
                    span (extClass "balance-after") $ text "2.924,50 €"

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
