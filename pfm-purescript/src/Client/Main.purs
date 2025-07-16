module Client.Main (main) where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Apply (lift2)
import Control.Parallel (parallel, sequential)
import Data.Array (length, uncons)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
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
  ( Account(..)
  , LedgerViewRow(LedgerViewRow)
  , Transaction(..)
  , User
  )
import Yoga.JSON as JSON

-- Data and Status types for parallel loading
data Status a
  = Loading
  | Failed String
  | Loaded a

derive instance Generic (Status a) _
instance Show a => Show (Status a) where
  show Loading = "Loading"
  show (Failed err) = "Failed: " <> err
  show (Loaded a) = "Loaded: " <> show a

type AppData =
  { users :: Array User
  , ledgerRows :: Array LedgerViewRow
  , accounts :: Array Account
  }

-- Dialog state types
type CreateDialogState =
  { description :: String
  , fromAccountId :: String
  , toAccountId :: String
  , amount :: String
  , date :: String
  }

type EditDialogState =
  { transactionId :: Int
  , description :: String
  , fromAccountId :: String
  , toAccountId :: String
  , amount :: String
  , date :: String
  }

data Dialog
  = CreateDialog CreateDialogState
  | EditDialog EditDialogState

data CreateDialogAction
  = CreateDescrChanged String
  | CreateFromAccountChanged String
  | CreateToAccountChanged String
  | CreateAmountChanged String
  | CreateDateChanged String

data EditDialogAction
  = EditDescrChanged String
  | EditFromAccountChanged String
  | EditToAccountChanged String
  | EditAmountChanged String
  | EditDateChanged String

type State =
  { data :: Status AppData
  , isDarkMode :: Boolean
  , dialog :: Maybe Dialog
  }

data Action
  = LoadAllData
  | ToggleDarkMode
  | Initialize
  | OpenCreateDialog
  | OpenEditDialog Int
  | CloseDialog
  | SaveDialog
  | CreateDialogChanged CreateDialogAction
  | EditDialogChanged EditDialogAction

-- Parallel data fetching function
fetchAllData :: Aff (Either String AppData)
fetchAllData = sequential $
  ( \users ledgerRows accounts ->
      case users, ledgerRows, accounts of
        Right u, Right l, Right a -> Right { users: u, ledgerRows: l, accounts: a }
        Left err, _, _ -> Left err
        _, Left err, _ -> Left err
        _, _, Left err -> Left err
  ) <$> parallel fetchUsers
    <*> parallel fetchLedgerView
    <*> parallel fetchAccounts

-- Individual fetch functions
fetchUsers :: Aff (Either String (Array User))
fetchUsers = do
  -- For now, return empty array since we don't have a users endpoint
  pure (Right [])

component :: forall q o m. MonadAff m => H.Component q InitArgs o m
component = H.mkComponent
  { initialState: \{ isDarkMode } ->
      { data: Loading
      , isDarkMode
      , dialog: Nothing
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

    -- Main content based on data loading status
    , case state.data of
        Loading ->
          HH.div
            [ HP.class_ (HH.ClassName "loading-message") ]
            [ HH.text "Loading application data..." ]
        Failed err ->
          HH.div
            [ HP.class_ (HH.ClassName "error-message") ]
            [ HH.text $ "Error: " <> err
            , HH.br_
            , HH.button
                [ HP.class_ (HH.ClassName "button")
                , HE.onClick \_ -> LoadAllData
                ]
                [ HH.text "Retry" ]
            ]
        Loaded appData ->
          HH.div_
            [ renderLedgerView state appData
            -- Dialog rendering
            , case state.dialog of
                Just dialog -> renderDialog dialog appData
                Nothing -> HH.text ""
            ]
    ]
  where

  renderLedgerView :: State -> AppData -> H.ComponentHTML Action () m
  renderLedgerView state appData =
    HH.div [ HP.class_ (HH.ClassName "transaction-list") ]
      [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header-title") ]
              [ HH.h3_ [ HH.text "Transactions" ]
              , HH.span [ HP.class_ (HH.ClassName "transaction-count") ]
                  [ HH.text $ show (length appData.ledgerRows) <> " transactions" ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-list__header-buttons") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "button")
                  , HE.onClick \_ -> LoadAllData
                  ]
                  [ HH.text "Refresh" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "button button--primary")
                  , HE.onClick \_ -> OpenCreateDialog
                  ]
                  [ HH.text "Create Transaction" ]
              ]
          ]
      , HH.ul [ HP.class_ (HH.ClassName "transaction-list__items") ]
          (map renderLedgerRow appData.ledgerRows)
      ]

  renderLedgerRow :: LedgerViewRow -> H.ComponentHTML Action () m
  renderLedgerRow (LedgerViewRow row) =
    let
      isPositive = row.flowAmount > 0.0
      amountClass =
        if isPositive then
          "transaction-item__amount transaction-item__amount--positive"
        else
          "transaction-item__amount transaction-item__amount--negative"
      amountSign = if isPositive then "+" else ""
    in
      HH.li
        [ HP.class_ (HH.ClassName "transaction-item")
        , HE.onClick \_ -> OpenEditDialog row.transactionId
        ]
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

  renderDialog :: Dialog -> AppData -> H.ComponentHTML Action () m
  renderDialog dialog appData =
    HH.dialog
      [ HP.class_ (HH.ClassName "transaction")
      , HP.id "transaction-dialog"
      ]
      [ HH.div
          [ HP.class_ (HH.ClassName "dialog-content") ]
          [ HH.div
              [ HP.class_ (HH.ClassName "dialog-header") ]
              [ HH.h2
                  [ HP.class_ (HH.ClassName "dialog-title") ]
                  [ HH.text $ case dialog of
                      CreateDialog _ -> "Create Transaction"
                      EditDialog _ -> "Edit Transaction"
                  ]
              ]
          , renderDialogForm dialog appData
          , HH.div
              [ HP.class_ (HH.ClassName "dialog-actions") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "button")
                  , HE.onClick \_ -> CloseDialog
                  ]
                  [ HH.text "Cancel" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "button button--primary")
                  , HE.onClick \_ -> SaveDialog
                  ]
                  [ HH.text "Save" ]
              ]
          ]
      ]

  renderDialogForm :: Dialog -> AppData -> H.ComponentHTML Action () m
  renderDialogForm dialog appData =
    case dialog of
      CreateDialog createState -> renderCreateForm createState appData
      EditDialog editState -> renderEditForm editState appData

  renderCreateForm :: CreateDialogState -> AppData -> H.ComponentHTML Action () m
  renderCreateForm createState appData =
    HH.form
      [ HP.class_ (HH.ClassName "dialog-form") ]
      [ makeTextField "Description" "description" createState.description
          (CreateDialogChanged <<< CreateDescrChanged)
      , makeAccountSelect "From Account" "from-account" createState.fromAccountId
          (CreateDialogChanged <<< CreateFromAccountChanged)
          appData.accounts
      , makeAccountSelect "To Account" "to-account" createState.toAccountId
          (CreateDialogChanged <<< CreateToAccountChanged)
          appData.accounts
      , makeTextField "Amount" "amount" createState.amount
          (CreateDialogChanged <<< CreateAmountChanged)
      , makeDateField "Date" "date" createState.date
          (CreateDialogChanged <<< CreateDateChanged)
      ]

  renderEditForm :: EditDialogState -> AppData -> H.ComponentHTML Action () m
  renderEditForm editState appData =
    HH.form
      [ HP.class_ (HH.ClassName "dialog-form") ]
      [ makeTextField "Description" "description" editState.description
          (EditDialogChanged <<< EditDescrChanged)
      , makeAccountSelect "From Account" "from-account" editState.fromAccountId
          (EditDialogChanged <<< EditFromAccountChanged)
          appData.accounts
      , makeAccountSelect "To Account" "to-account" editState.toAccountId
          (EditDialogChanged <<< EditToAccountChanged)
          appData.accounts
      , makeTextField "Amount" "amount" editState.amount
          (EditDialogChanged <<< EditAmountChanged)
      , makeDateField "Date" "date" editState.date
          (EditDialogChanged <<< EditDateChanged)
      ]

  makeTextField :: String -> String -> String -> (String -> Action) -> H.ComponentHTML Action () m
  makeTextField label fieldId value onChange =
    HH.div
      [ HP.class_ (HH.ClassName "field") ]
      [ HH.label
          [ HP.class_ (HH.ClassName "field__label")
          , HP.for fieldId
          ]
          [ HH.text label ]
      , HH.input
          [ HP.class_ (HH.ClassName "field__input")
          , HP.id fieldId
          , HP.type_ HP.InputText
          , HP.value value
          , HE.onValueInput onChange
          ]
      ]

  makeDateField :: String -> String -> String -> (String -> Action) -> H.ComponentHTML Action () m
  makeDateField label fieldId value onChange =
    HH.div
      [ HP.class_ (HH.ClassName "field") ]
      [ HH.label
          [ HP.class_ (HH.ClassName "field__label")
          , HP.for fieldId
          ]
          [ HH.text label ]
      , HH.input
          [ HP.class_ (HH.ClassName "field__input")
          , HP.id fieldId
          , HP.type_ HP.InputDatetimeLocal
          , HP.value value
          , HE.onValueInput onChange
          ]
      ]

  makeAccountSelect :: String -> String -> String -> (String -> Action) -> Array Account -> H.ComponentHTML Action () m
  makeAccountSelect label fieldId value onChange accounts =
    HH.div
      [ HP.class_ (HH.ClassName "field") ]
      [ HH.label
          [ HP.class_ (HH.ClassName "field__label")
          , HP.for fieldId
          ]
          [ HH.text label ]
      , HH.select
          [ HP.class_ (HH.ClassName "field__select")
          , HP.id fieldId
          , HP.value value
          , HE.onValueInput onChange
          ]
          ( [ HH.option
                [ HP.value ""
                , HP.disabled true
                ]
                [ HH.text "Select an account..." ]
            ] <> map renderAccountOption accounts
          )
      ]

  renderAccountOption :: Account -> H.ComponentHTML Action () m
  renderAccountOption (Account account) =
    HH.option
      [ HP.value (show account.accountId) ]
      [ HH.text account.name ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load all data in parallel on startup
    handleAction LoadAllData
  LoadAllData -> do
    H.modify_ \s -> s { data = Loading }
    result <- H.liftAff fetchAllData
    case result of
      Left err -> H.modify_ \s -> s { data = Failed err }
      Right appData -> H.modify_ \s -> s { data = Loaded appData }
  ToggleDarkMode -> do
    state <- H.get
    let newDarkMode = not state.isDarkMode
    H.modify_ \s -> s { isDarkMode = newDarkMode }
    -- Call JavaScript to toggle theme
    liftEffect $ toggleTheme unit

  OpenCreateDialog -> do
    let
      initialState =
        { description: ""
        , fromAccountId: ""
        , toAccountId: ""
        , amount: ""
        , date: ""
        }
    H.modify_ \s -> s { dialog = Just (CreateDialog initialState) }
    liftEffect $ dialogShow "transaction-dialog"

  OpenEditDialog transactionId -> do
    -- Find the transaction to edit from ledger rows
    state <- H.get
    case state.data of
      Loaded appData -> do
        case findTransaction transactionId appData.ledgerRows of
          Just (LedgerViewRow row) -> do
            let
              editState =
                { transactionId: row.transactionId
                , description: row.description
                , fromAccountId: show row.fromAccountId
                , toAccountId: show row.toAccountId
                , amount: show row.flowAmount
                , date: row.date
                }
            H.modify_ \s -> s { dialog = Just (EditDialog editState) }
            liftEffect $ dialogShow "transaction-dialog"
          Nothing -> pure unit
      _ -> pure unit

  CloseDialog -> do
    H.modify_ \s -> s { dialog = Nothing }
    liftEffect $ dialogClose "transaction-dialog"

  SaveDialog -> do
    state <- H.get
    case state.dialog of
      Just (CreateDialog createState) -> do
        -- TODO: Implement create transaction API call
        liftEffect $ dialogClose "transaction-dialog"
        H.modify_ \s -> s { dialog = Nothing }
        -- Refresh all data
        handleAction LoadAllData
      Just (EditDialog editState) -> do
        -- TODO: Implement edit transaction API call
        liftEffect $ dialogClose "transaction-dialog"
        H.modify_ \s -> s { dialog = Nothing }
        -- Refresh all data
        handleAction LoadAllData
      Nothing -> pure unit

  CreateDialogChanged createAction -> do
    state <- H.get
    case state.dialog of
      Just (CreateDialog createState) -> do
        let newState = updateCreateState createAction createState
        H.modify_ \s -> s { dialog = Just (CreateDialog newState) }
      _ -> pure unit

  EditDialogChanged editAction -> do
    state <- H.get
    case state.dialog of
      Just (EditDialog editState) -> do
        let newState = updateEditState editAction editState
        H.modify_ \s -> s { dialog = Just (EditDialog newState) }
      _ -> pure unit

findTransaction :: Int -> Array LedgerViewRow -> Maybe LedgerViewRow
findTransaction transactionId rows =
  case uncons rows of
    Nothing -> Nothing
    Just { head: row, tail: rest } ->
      case row of
        LedgerViewRow r ->
          if r.transactionId == transactionId then Just row
          else findTransaction transactionId rest

updateCreateState :: CreateDialogAction -> CreateDialogState -> CreateDialogState
updateCreateState action state =
  case action of
    CreateDescrChanged desc -> state { description = desc }
    CreateFromAccountChanged from -> state { fromAccountId = from }
    CreateToAccountChanged to -> state { toAccountId = to }
    CreateAmountChanged amount -> state { amount = amount }
    CreateDateChanged date -> state { date = date }

updateEditState :: EditDialogAction -> EditDialogState -> EditDialogState
updateEditState action state =
  case action of
    EditDescrChanged desc -> state { description = desc }
    EditFromAccountChanged from -> state { fromAccountId = from }
    EditToAccountChanged to -> state { toAccountId = to }
    EditAmountChanged amount -> state { amount = amount }
    EditDateChanged date -> state { date = date }

baseUrl :: String
baseUrl = "http://localhost:8081"

fetchLedgerView :: Aff (Either String (Array LedgerViewRow))
fetchLedgerView = do
  -- Fetch ledger view for checking account (ID = 2) from the API
  result <- AX.get ResponseFormat.string (baseUrl <> "/accounts/2/ledger")
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (ledgerRows :: Array LedgerViewRow) ->
          pure $ Right ledgerRows

fetchAccounts :: Aff (Either String (Array Account))
fetchAccounts = do
  -- Fetch accounts from the API
  result <- AX.get ResponseFormat.string (baseUrl <> "/accounts")
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (accounts :: Array Account) ->
          pure $ Right accounts

-- Foreign import to call JavaScript theme toggle
foreign import toggleTheme :: Unit -> Effect Unit

-- Foreign imports for dialog control
foreign import dialogShow :: String -> Effect Unit
foreign import dialogClose :: String -> Effect Unit

type InitArgs = { isDarkMode :: Boolean }

main :: InitArgs -> Effect Unit
main initArgs = do
  log "[CLIENT] Booting up..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component initArgs body

