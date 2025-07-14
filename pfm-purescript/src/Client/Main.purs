module Client.Main (main) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Data.Array (length)
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
  ( LedgerViewRow(LedgerViewRow)
  , User
  )
import Yoga.JSON as JSON


type State =
  { users :: Array User
  , ledgerRows :: Array LedgerViewRow
  , loading :: Boolean
  , error :: Maybe String
  , isDarkMode :: Boolean
  }

data Action
  = LoadUsers
  | LoadLedgerView
  | ToggleDarkMode
  | Initialize

component :: forall q o m. MonadAff m => H.Component q InitArgs o m
component = H.mkComponent
  { initialState: \{ isDarkMode } ->
      { users: []
      , ledgerRows: []
      , loading: false
      , error: Nothing
      , isDarkMode
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
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

    -- Ledger section  
    , HH.div
        [ HP.class_ (HH.ClassName "section") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "section-header") ]
            [ HH.h2
                [ HP.class_ (HH.ClassName "section-title") ]
                [ HH.text "Transactions" ]
            , HH.button
                [ HP.class_ (HH.ClassName "refresh-button")
                , HE.onClick \_ -> LoadLedgerView
                , HP.disabled state.loading
                ]
                [ HH.text if state.loading then "Loading..." else "Refresh" ]
            ]
        , case state.error of
            Just err -> HH.div
              [ HP.class_ (HH.ClassName "error-message") ]
              [ HH.text $ "Error: " <> err ]
            Nothing -> HH.text ""
        , if state.loading then
            HH.div
              [ HP.class_ (HH.ClassName "loading-message") ]
              [ HH.text "Loading ledger..." ]
          else
            renderLedgerView state
        ]
    ]
  where

  renderLedgerView :: State -> H.ComponentHTML Action () m
  renderLedgerView state =
    HH.div [ HP.class_ (HH.ClassName "transaction-list") ]
      [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header-title") ]
              [ HH.h3_ [ HH.text "Transactions" ]
              , HH.span [ HP.class_ (HH.ClassName "transaction-count") ] 
                  [ HH.text $ show (length state.ledgerRows) <> " transactions" ]
              ]
          ]
      , HH.ul [ HP.class_ (HH.ClassName "transaction-list__items") ]
          (map renderLedgerRow state.ledgerRows)
      ]

  renderLedgerRow :: LedgerViewRow -> H.ComponentHTML Action () m
  renderLedgerRow (LedgerViewRow row) =
    let
      isPositive = row.flowAmount > 0.0
      amountClass = if isPositive then
                      "transaction-item__amount transaction-item__amount--positive"
                    else  
                      "transaction-item__amount transaction-item__amount--negative"
      amountSign = if isPositive then "+" else ""
    in
    HH.li [ HP.class_ (HH.ClassName "transaction-item") ]
      [ HH.div [ HP.class_ (HH.ClassName "transaction-item__row") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-item__main-content") ]
              [ HH.div [ HP.class_ (HH.ClassName "transaction-item__details") ]
                  [ HH.div [ HP.class_ (HH.ClassName "transaction-item__description") ]
                      [ HH.text row.description ]
                  , HH.div [ HP.class_ (HH.ClassName "transaction-item__accounts") ]
                      [ HH.text $ row.fromAccountName <> " → " <> row.toAccountName ]
                  ]
              , HH.div [ HP.class_ (HH.ClassName "transaction-item__date") ]
                  [ HH.text row.date ]
              , HH.div [ HP.class_ (HH.ClassName amountClass) ]
                  [ HH.text $ amountSign <> show row.flowAmount <> " €" ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-item__balance-column") ]
              [ HH.div [ HP.class_ (HH.ClassName "transaction-item__balance-movement") ]
                  [ HH.span [ HP.class_ (HH.ClassName "balance-after") ] 
                      [ HH.text $ show row.runningBalance <> " €" ]
                  ]
              ]
          ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load ledger view on startup
    handleAction LoadLedgerView
  LoadUsers -> pure unit -- Keep as reference but do nothing
  LoadLedgerView -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff fetchLedgerView
    case result of
      Left err -> H.modify_ \s -> s { loading = false, error = Just err }
      Right ledgerRows -> H.modify_ \s -> s { loading = false, ledgerRows = ledgerRows, error = Nothing }
  ToggleDarkMode -> do
    state <- H.get
    let newDarkMode = not state.isDarkMode
    H.modify_ \s -> s { isDarkMode = newDarkMode }
    -- Call JavaScript to toggle theme
    liftEffect $ toggleTheme unit

fetchLedgerView :: Aff (Either String (Array LedgerViewRow))
fetchLedgerView = do
  -- Fetch ledger view for checking account (ID = 2) from the API
  result <- AX.get ResponseFormat.string "http://localhost:8080/accounts/2/ledger"
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (ledgerRows :: Array LedgerViewRow) -> 
          pure $ Right ledgerRows

-- Foreign import to call JavaScript theme toggle
foreign import toggleTheme :: Unit -> Effect Unit

type InitArgs = { isDarkMode :: Boolean }

main :: InitArgs -> Effect Unit
main initArgs = do
  log "[CLIENT] Booting up..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component initArgs body

