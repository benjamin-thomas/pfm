module Client.Main (main) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Shared.Types
  ( Transaction(Transaction)
  , User
  )
import Yoga.JSON as JSON

type State =
  { users :: Array User
  , transactions :: Array Transaction
  , loading :: Boolean
  , error :: Maybe String
  , isDarkMode :: Boolean
  }

data Action
  = LoadUsers
  | LoadTransactions
  | ToggleDarkMode

component :: forall q o m. MonadAff m => H.Component q InitArgs o m
component = H.mkComponent
  { initialState: \{ isDarkMode } ->
      { users: []
      , transactions: mockTransactions -- Start with mock data
      , loading: false
      , error: Nothing
      , isDarkMode
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "container") ]
    [ HH.h1_ [ HH.text "PFM - PureScript" ]

    -- Dark mode toggle button
    , HH.button
        [ HP.class_ (HH.ClassName "theme-toggle")
        , HE.onClick \_ -> ToggleDarkMode
        , HP.title (if state.isDarkMode then "Switch to light mode" else "Switch to dark mode")
        ]
        [ HH.text (if state.isDarkMode then "☀️" else "🌙") ]

    -- Transactions section  
    , HH.div
        [ HP.class_ (HH.ClassName "section") ]
        [ HH.h2
            [ HP.class_ (HH.ClassName "section-title") ]
            [ HH.text "Transactions" ]
        , HH.div
            [ HP.class_ (HH.ClassName "transaction-list") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "transaction-list__header") ]
                [ HH.h3_ [ HH.text "Recent Transactions" ]
                ]
            , HH.ul
                [ HP.class_ (HH.ClassName "transaction-list__items") ]
                (map renderTransaction state.transactions)
            ]
        ]
    ]
  where
  renderTransaction :: Transaction -> H.ComponentHTML Action () m
  renderTransaction (Transaction tx) =
    HH.li
      [ HP.class_ (HH.ClassName "transaction-item") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "transaction-item__row") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "transaction-item__main-content") ]
              [ HH.div
                  [ HP.class_ (HH.ClassName "transaction-item__details") ]
                  [ HH.div
                      [ HP.class_ (HH.ClassName "transaction-item__description") ]
                      [ HH.text tx.description ]
                  , HH.div
                      [ HP.class_ (HH.ClassName "transaction-item__accounts") ]
                      [ HH.text $ tx.fromAccountName <> " → " <> tx.toAccountName ]
                  ]
              , HH.div
                  [ HP.class_ (HH.ClassName "transaction-item__date") ]
                  [ HH.text tx.date ]
              ]
          , HH.div
              [ HP.class_ $ HH.ClassName $ "transaction-item__amount" <>
                  if tx.amount >= 0.0 then " transaction-item__amount--positive" else " transaction-item__amount--negative"
              ]
              [ HH.text $ "$" <> show tx.amount ]
          ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  LoadUsers -> pure unit -- Keep as reference but do nothing
  LoadTransactions -> do
    -- For now, just refresh with mock data
    H.modify_ \s -> s { transactions = mockTransactions }
  ToggleDarkMode -> do
    state <- H.get
    let newDarkMode = not state.isDarkMode
    H.modify_ \s -> s { isDarkMode = newDarkMode }
    -- Call JavaScript to toggle theme
    liftEffect $ toggleTheme unit

fetchUsers :: Aff (Either String (Array User))
fetchUsers = do
  result <- AX.get ResponseFormat.string "http://localhost:8080/users"
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right users -> pure $ Right users

-- Foreign import to call JavaScript theme toggle
foreign import toggleTheme :: Unit -> Effect Unit

type InitArgs = { isDarkMode :: Boolean }

main :: InitArgs -> Effect Unit
main initArgs = do
  log "[CLIENT] Booting up..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component initArgs body

-- Mock transaction data
mockTransactions :: Array Transaction
mockTransactions =
  [ Transaction
      { id: Just 1
      , fromAccountId: 1
      , fromAccountName: "Checking"
      , toAccountId: 2
      , toAccountName: "Groceries"
      , date: "2024-01-15"
      , description: "Whole Foods Market"
      , amount: -125.50
      }
  , Transaction
      { id: Just 2
      , fromAccountId: 3
      , fromAccountName: "Salary"
      , toAccountId: 1
      , toAccountName: "Checking"
      , date: "2024-01-14"
      , description: "Monthly Salary"
      , amount: 3500.00
      }
  , Transaction
      { id: Just 3
      , fromAccountId: 1
      , fromAccountName: "Checking"
      , toAccountId: 4
      , toAccountName: "Utilities"
      , date: "2024-01-12"
      , description: "Electric Bill - January"
      , amount: -89.00
      }
  , Transaction
      { id: Just 4
      , fromAccountId: 1
      , fromAccountName: "Checking"
      , toAccountId: 5
      , toAccountName: "Entertainment"
      , date: "2024-01-10"
      , description: "Netflix Subscription"
      , amount: -15.99
      }
  , Transaction
      { id: Just 5
      , fromAccountId: 1
      , fromAccountName: "Checking"
      , toAccountId: 6
      , toAccountName: "Transportation"
      , date: "2024-01-08"
      , description: "Gas Station - Shell"
      , amount: -45.00
      }
  ]