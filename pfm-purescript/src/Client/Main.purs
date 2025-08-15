module Client.Main (main) where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader as RequestHeader
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web as AX
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.Int (toNumber)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as Number
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Shared.Types (Account(..), AccountBalanceRead(..), LedgerViewRow(LedgerViewRow), User, Suggestion(..), SuggestedAccount(..))
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent as MouseEvent
import Yoga.JSON as JSON

-- Type-safe wrapper for API base URL
newtype ApiBaseUrl = ApiBaseUrl String

derive instance newtypeApiBaseUrl :: Newtype ApiBaseUrl _

instance semigroupApiBaseUrl :: Semigroup ApiBaseUrl where
  append (ApiBaseUrl a) (ApiBaseUrl b) = ApiBaseUrl (a <> b)

-- Helper to create ApiBaseUrl from String
mkApiBaseUrl :: String -> ApiBaseUrl
mkApiBaseUrl = ApiBaseUrl

-- Helper to append string to ApiBaseUrl
appendPath :: ApiBaseUrl -> String -> ApiBaseUrl
appendPath (ApiBaseUrl base) path = ApiBaseUrl (base <> path)

-- Helper to extract URL string for HTTP requests
unApiBaseUrl :: ApiBaseUrl -> String
unApiBaseUrl (ApiBaseUrl url) = url

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
  , balances :: Array AccountBalanceRead
  , suggestions :: Array Suggestion
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

data SearchBy
  = ByDescr String
  | BySimilarTo ContextMenuData

type SearchForm =
  { searchBy :: SearchBy
  , minAmount :: String
  , maxAmount :: String
  , filterUnknownExpenses :: Boolean
  }

type ContextMenuData =
  { transactionId :: Int
  , x :: Number
  , y :: Number
  , soundexDescr :: String
  , descr :: String
  }

data FilterAction
  = FilterDescriptionChanged String
  | FilterMinAmountChanged String
  | FilterMaxAmountChanged String
  | FilterUnknownExpensesToggled
  | ClearFilters

type State =
  { data :: Status AppData
  , isDarkMode :: Boolean
  , dialog :: Maybe Dialog
  , apiBaseUrl :: ApiBaseUrl
  , searchForm :: SearchForm
  , debounceTimerId :: Maybe H.ForkId
  , contextMenu :: Maybe ContextMenuData
  , scrollY :: Maybe Number
  , last_SSE_Message :: Maybe String
  }

data Action
  = LoadAllData
  | LoadLedgerView
  | ToggleDarkMode
  | Initialize
  | OpenCreateDialog
  | OpenEditDialog Int
  | CloseDialog
  | SaveDialog
  | DeleteTransaction
  | CreateDialogChanged CreateDialogAction
  | EditDialogChanged EditDialogAction
  | FilterChanged FilterAction
  | DebouncedFilterUpdate
  | ShowContextMenu Event { soundexDescr :: String, descr :: String }
  | HideContextMenu
  | ContextMenuAction String
  | ApplySuggestionItem Event
      { transaction :: -- TODO: pass the LedgerViewRow type all through out
          { transactionId :: Int
          , fromAccountId :: Int
          , dateUnix :: Int
          , descr :: String
          , flowCents :: Int
          }
      , accountId :: Int
      }
  | ApplyAllSuggestions (Array { transactionId :: Int, accountId :: Int })

  {-

The String param is the value of "data":

$ curl -N http://localhost:8082/events
event: connected
data: {"message": "Pure PureScript SSE established"}

event: ping
data: {"time:"(Instant (Milliseconds 1755250926712.0))}

event: ping
data: {"time:"(Instant (Milliseconds 1755250928712.0))}

I'm not differentiating any event types currently.

   -}
  | GotSSE_Message String

-- Parallel data fetching function
fetchAllData :: ApiBaseUrl -> SearchForm -> Aff (Either String AppData)
fetchAllData apiBaseUrl searchForm = do
  results <- sequential $
    { users: _, ledgerRows: _, accounts: _, balances: _, suggestions: _ }
      <$> parallel fetchUsers
      <*> parallel (fetchLedgerView apiBaseUrl searchForm)
      <*> parallel (fetchAccounts apiBaseUrl)
      <*> parallel (fetchBalances apiBaseUrl)
      <*> parallel (fetchSuggestions apiBaseUrl)

  pure $ do
    users <- results.users
    ledgerRows <- results.ledgerRows
    accounts <- results.accounts
    balances <- results.balances
    suggestions <- results.suggestions
    pure { users, ledgerRows, accounts, balances, suggestions }

-- Individual fetch functions
fetchUsers :: Aff (Either String (Array User))
fetchUsers = do
  -- For now, return empty array since we don't have a users endpoint
  pure (Right [])

component :: forall q o m. MonadAff m => H.Component q InitArgs o m
component = H.mkComponent
  { initialState: \{ isDarkMode, apiBaseUrl } ->
      { data: Loading
      , isDarkMode
      , dialog: Nothing
      , apiBaseUrl: mkApiBaseUrl apiBaseUrl
      , searchForm:
          { searchBy: ByDescr ""
          , minAmount: ""
          , maxAmount: ""
          , filterUnknownExpenses: false
          }
      , debounceTimerId: Nothing
      , contextMenu: Nothing
      , scrollY: Nothing
      , last_SSE_Message: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

-- | Collect all available suggestions from ledger rows and suggestions data
-- | Returns an array of { transactionId, accountId } pairs for batch updating
collectAllSuggestions :: Array LedgerViewRow -> Array Suggestion -> Array { transactionId :: Int, accountId :: Int }
collectAllSuggestions ledgerRows suggestions =
  Array.mapMaybe extractSuggestion ledgerRows
  where
  extractSuggestion :: LedgerViewRow -> Maybe { transactionId :: Int, accountId :: Int }
  extractSuggestion (LedgerViewRow row) = do
    -- Only suggest for transactions going to Unknown_EXPENSE (account 6)
    if row.toAccountId == 6 then do
      -- Find matching suggestion by soundex
      suggestion <- Array.find (\(Suggestion s) -> s.soundexDescr == row.soundexDescr) suggestions
      case suggestion of
        Suggestion s -> do
          -- Get the first (most likely) suggested account
          firstSuggestion <- Array.head s.suggestedAccounts
          case firstSuggestion of
            SuggestedAccount account ->
              Just { transactionId: row.transactionId, accountId: account.accountId }
    else
      Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "container") ]
    [ HH.h1_ [ HH.text "PFM - PureScript" ]

    , HH.pre_
        [ HH.text $ fromMaybe "No SSE message received yet" state.last_SSE_Message ]
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
        Loaded appData -> HH.span
          []
          [ HH.div
              [ HP.class_ (HH.ClassName "section") ]
              [ renderBalanceCards appData
              ]
          , HH.div
              [ HP.class_ (HH.ClassName "section") ]
              [ renderLedgerView appData ]
          , case state.dialog of
              Just dialog -> renderDialog dialog appData
              Nothing -> HH.text ""
          , case state.contextMenu of
              Just contextData -> renderContextMenu contextData
              Nothing -> HH.text ""
          ]
    ]
  where

  renderBalanceCards :: AppData -> H.ComponentHTML Action () m
  renderBalanceCards appData =
    HH.div []
      [ HH.h2 [ HP.class_ (HH.ClassName "section-title") ]
          [ HH.text "Balances" ]
      , HH.div
          [ HP.class_ (HH.ClassName "balances") ]
          (map renderBalanceCard appData.balances)
      ]

  renderBalanceCard :: AccountBalanceRead -> H.ComponentHTML Action () m
  renderBalanceCard (AccountBalanceRead balance) =
    let
      colorAccent =
        if balance.categoryName == "Assets" then
          "#3498db"
        else if balance.categoryName == "Expenses" then
          "#e74c3c"
        else
          "#9b59b6"

      -- Format amount with 2 decimal places
      formattedAmount =
        let
          intPart = balance.accountBalance / 100
          decPart =
            if balance.accountBalance >= 0 then balance.accountBalance `mod` 100
            else -(balance.accountBalance `mod` 100)
          decStr = if decPart < 10 then "0" <> show decPart else show decPart
        in
          show intPart <> "." <> decStr <> " €"
    in
      HH.div
        [ HP.class_ (HH.ClassName "balance-card")
        , HP.style ("border-left-color: " <> colorAccent)
        ]
        [ HH.div
            [ HP.class_ (HH.ClassName "balance-card__category") ]
            [ HH.text balance.categoryName ]
        , HH.div
            [ HP.class_ (HH.ClassName "balance-card__account") ]
            [ HH.text balance.accountName ]
        , HH.div
            [ HP.class_ (HH.ClassName "balance-card__amount") ]
            [ HH.text formattedAmount ]
        ]

  renderLedgerView :: AppData -> H.ComponentHTML Action () m
  renderLedgerView appData =
    HH.div [ HP.class_ (HH.ClassName "transaction-list") ]
      [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-list__header-title") ]
              [ HH.h3_ [ HH.text "Transactions" ]
              , HH.span [ HP.class_ (HH.ClassName "transaction-count") ]
                  [ HH.text $ show (Array.length appData.ledgerRows) <> " transactions" ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-list__header-buttons") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "button button--secondary")
                  , HE.onClick \_ -> ApplyAllSuggestions (collectAllSuggestions appData.ledgerRows appData.suggestions)
                  ]
                  [ HH.text "💡 Apply All Suggestions" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "button button--primary")
                  , HE.onClick \_ -> OpenCreateDialog
                  ]
                  [ HH.text "Add Transaction" ]
              ]
          ]
      , renderSearchForm state.searchForm
      , HH.ul [ HP.class_ (HH.ClassName "transaction-list__items") ]
          (map (renderLedgerRow appData.suggestions) appData.ledgerRows)
      ]

  renderSearchForm :: SearchForm -> H.ComponentHTML Action () m
  renderSearchForm searchForm =
    HH.div [ HP.class_ (HH.ClassName "transaction-search") ]
      [ HH.div [ HP.class_ (HH.ClassName "transaction-search__row") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-search__field") ]
              [ HH.label [ HP.for "search-description" ] [ HH.text "Description" ]
              , HH.input
                  [ HP.type_ HP.InputText
                  , HP.id "search-description"
                  , HE.onValueInput (\str -> FilterChanged (FilterDescriptionChanged str))
                  , HP.value
                      ( case searchForm.searchBy of
                          ByDescr desc -> desc
                          BySimilarTo _ -> ""
                      )
                  , HP.placeholder "Search by description"
                  , HP.autocomplete HP.AutocompleteOff
                  , HP.class_ (HH.ClassName "transaction-search__input")
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-search__field") ]
              [ HH.label [ HP.for "search-amount-min" ] [ HH.text "Min Amount" ]
              , HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.id "search-amount-min"
                  , HP.placeholder "40.00"
                  , HP.min 0.0
                  , HE.onValueInput (\str -> FilterChanged (FilterMinAmountChanged str))
                  , HP.value searchForm.minAmount
                  , HP.class_ (HH.ClassName "transaction-search__input")
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-search__field") ]
              [ HH.label [ HP.for "search-amount-max" ] [ HH.text "Max Amount" ]
              , HH.input
                  [ HP.type_ HP.InputNumber
                  , HP.id "search-amount-max"
                  , HP.placeholder "100.00"
                  , HP.min 0.0
                  , HE.onValueInput (\str -> FilterChanged (FilterMaxAmountChanged str))
                  , HP.value searchForm.maxAmount
                  , HP.class_ (HH.ClassName "transaction-search__input")
                  ]
              ]
          ]
      , HH.div [ HP.class_ (HH.ClassName "transaction-search__row transaction-search__row--bottom") ]
          [ HH.div [ HP.class_ (HH.ClassName "transaction-search__field transaction-search__field--checkbox") ]
              [ HH.label [ HP.class_ (HH.ClassName "checkbox-container") ]
                  [ HH.input
                      [ HP.type_ HP.InputCheckbox
                      , HP.checked searchForm.filterUnknownExpenses
                      , HE.onClick \_ -> FilterChanged FilterUnknownExpensesToggled
                      ]
                  , HH.span
                      [ HP.class_ (HH.ClassName "checkbox-label")
                      , HP.style "margin-left: 3px"
                      ]
                      [ HH.text "Unknown expenses" ]
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "transaction-search__field--button") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "search-clear-button")
                  , HP.type_ HP.ButtonButton
                  , HP.id "form-clear-button"
                  , HE.onClick \_ -> FilterChanged ClearFilters
                  ]
                  [ HH.text "Clear" ]
              ]
          ]
      , case searchForm.searchBy of
          BySimilarTo contextMenuData ->
            HH.div [ HP.class_ (HH.ClassName "similar-transactions-info") ]
              [ HH.span [ HP.class_ (HH.ClassName "similar-transactions-label") ]
                  [ HH.text "Displaying transactions similar to:" ]
              , HH.span [ HP.class_ (HH.ClassName "similar-transactions-value") ]
                  [ HH.text contextMenuData.descr ]
              ]
          ByDescr _ -> HH.text ""
      ]

  renderLedgerRow :: Array Suggestion -> LedgerViewRow -> H.ComponentHTML Action () m
  renderLedgerRow suggestions (LedgerViewRow row) =
    let
      isPositive = row.flowCents > 0
      amountClass =
        if isPositive then
          "transaction-item__amount transaction-item__amount--positive"
        else
          "transaction-item__amount transaction-item__amount--negative"
      amountSign = if isPositive then "+" else ""

      -- Find suggestion for this transaction
      maybeSuggestion = findSuggestionForTransaction row.soundexDescr suggestions

    in
      HH.li
        [ HP.class_ (HH.ClassName "transaction-item")
        , HE.onClick \_ -> OpenEditDialog row.transactionId
        , HE.handler (EventType "contextmenu") \event -> do
            (ShowContextMenu event { soundexDescr: row.soundexDescr, descr: row.descr })
        ]
        [ HH.div [ HP.class_ (HH.ClassName "transaction-item__row") ]
            [ HH.div [ HP.class_ (HH.ClassName "transaction-item__main-content") ]
                [ HH.div [ HP.class_ (HH.ClassName "transaction-item__details") ]
                    [ HH.div [ HP.class_ (HH.ClassName "transaction-item__description") ]
                        [ HH.text row.descr ]
                    , HH.div [ HP.class_ (HH.ClassName "transaction-item__accounts") ]
                        [ HH.text $ row.fromAccountName <> " → " <> row.toAccountName ]
                    ]
                , HH.div [ HP.class_ (HH.ClassName "transaction-item__date") ]
                    [ HH.text row.date ]
                , HH.div [ HP.class_ (HH.ClassName amountClass) ]
                    [ HH.text $ amountSign <> row.flow <> " €" ]
                ]
            , HH.div [ HP.class_ (HH.ClassName "transaction-item__balance-column") ]
                [ HH.div [ HP.class_ (HH.ClassName "transaction-item__balance-movement") ]
                    [ HH.span [ HP.class_ (HH.ClassName "balance-before") ]
                        [ HH.text $ row.priorBalance <> " €" ]
                    , HH.span [ HP.class_ (HH.ClassName "arrow-icon") ]
                        [ HH.text " → " ]
                    , HH.span [ HP.class_ (HH.ClassName "balance-after") ]
                        [ HH.text $ row.runningBalance <> " €" ]
                    ]
                ]
            ]
        -- Render suggestion if available and transaction goes to Unknown_EXPENSE
        , case maybeSuggestion of
            Just suggestion | row.toAccountName == "Unknown_EXPENSE" ->
              renderTransactionSuggestion
                { transactionId: row.transactionId
                , fromAccountId: row.fromAccountId
                , dateUnix: row.dateUnix
                , descr: row.descr
                , flowCents: row.flowCents
                }
                suggestion
            _ -> HH.text ""
        ]

  -- Helper function to find suggestion for a transaction based on soundex
  findSuggestionForTransaction :: String -> Array Suggestion -> Maybe Suggestion
  findSuggestionForTransaction soundexDescr suggestions =
    Array.find (\(Suggestion s) -> s.soundexDescr == soundexDescr) suggestions

  -- Render the yellow suggestion bar for a transaction
  renderTransactionSuggestion :: { transactionId :: Int, fromAccountId :: Int, dateUnix :: Int, descr :: String, flowCents :: Int } -> Suggestion -> H.ComponentHTML Action () m
  renderTransactionSuggestion txnRow (Suggestion suggestion) =
    case Array.head suggestion.suggestedAccounts of
      Nothing -> HH.text ""
      Just (SuggestedAccount account) ->
        HH.div
          [ HP.class_ (HH.ClassName "suggestion-container") ]
          [ HH.div [ HP.class_ (HH.ClassName "suggestion-text") ]
              [ HH.span [ HP.class_ (HH.ClassName "suggestion-icon") ] [ HH.text "💡" ]
              , HH.span []
                  [ HH.text "Suggested category: "
                  , HH.strong [] [ HH.text account.accountName ]
                  ]
              ]
          , HH.div [ HP.class_ (HH.ClassName "suggestion-actions") ]
              [ HH.button
                  [ HP.class_ (HH.ClassName "suggestion-btn suggestion-btn-apply")
                  , HE.handler (EventType "click") \event -> do
                      ApplySuggestionItem event { transaction: txnRow, accountId: account.accountId }
                  ]
                  [ HH.text "Apply" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "suggestion-btn suggestion-btn-ignore") ]
                  [ HH.text "Ignore" ]
              , HH.button
                  [ HP.class_ (HH.ClassName "suggestion-btn suggestion-btn-ai") ]
                  [ HH.text "Ask AI ✨" ]
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
              , case dialog of
                  CreateDialog _ -> HH.text ""
                  EditDialog _ ->
                    HH.div
                      [ HP.class_ (HH.ClassName "dialog-menu") ]
                      [ HH.button
                          [ HP.class_ (HH.ClassName "menu-button")
                          , HP.attr (HH.AttrName "aria-label") "More options"
                          ]
                          [ HH.text "⋮" ]
                      , HH.div
                          [ HP.class_ (HH.ClassName "menu-dropdown") ]
                          [ HH.button
                              [ HP.class_ (HH.ClassName "menu-item")
                              , HE.onClick \_ -> DeleteTransaction
                              ]
                              [ HH.text "Delete" ]
                          , HH.button
                              [ HP.class_ (HH.ClassName "menu-item menu-item--disabled")
                              ]
                              [ HH.text "Duplicate" ]
                          ]
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
      [ makeTextField
          { label: "Description"
          , fieldId: "description"
          , value: createState.description
          , onChange: CreateDialogChanged <<< CreateDescrChanged
          , autofocus: false
          }
      , makeAccountSelect "From Account" "from-account" createState.fromAccountId
          (CreateDialogChanged <<< CreateFromAccountChanged)
          appData.accounts
      , makeAccountSelect "To Account" "to-account" createState.toAccountId
          (CreateDialogChanged <<< CreateToAccountChanged)
          appData.accounts
      , makeNumberField
          { label: "Amount"
          , fieldId: "amount"
          , value: createState.amount
          , onChange: CreateDialogChanged <<< CreateAmountChanged
          , autofocus: false
          }
      , makeDateField "Date" "date" createState.date
          (CreateDialogChanged <<< CreateDateChanged)
      ]

  renderEditForm :: EditDialogState -> AppData -> H.ComponentHTML Action () m
  renderEditForm editState appData =
    HH.form
      [ HP.class_ (HH.ClassName "dialog-form") ]
      [ makeTextField
          { label: "Description"
          , fieldId: "description"
          , value: editState.description
          , onChange: EditDialogChanged <<< EditDescrChanged
          , autofocus: false
          }
      , makeAccountSelect "From Account" "from-account" editState.fromAccountId
          (EditDialogChanged <<< EditFromAccountChanged)
          appData.accounts
      , makeAccountSelect "To Account" "to-account" editState.toAccountId
          (EditDialogChanged <<< EditToAccountChanged)
          appData.accounts
      , makeNumberField
          { label: "Amount"
          , fieldId: "amount"
          , value: editState.amount
          , onChange: EditDialogChanged <<< EditAmountChanged
          , autofocus: true
          }
      , makeDateField "Date" "date" editState.date
          (EditDialogChanged <<< EditDateChanged)
      ]

  makeTextField
    :: { label :: String
       , fieldId :: String
       , value :: String
       , onChange :: String -> Action
       , autofocus :: Boolean
       }
    -> H.ComponentHTML Action () m
  makeTextField { label, fieldId, value, onChange, autofocus } =
    HH.div
      [ HP.class_ (HH.ClassName "field") ]
      [ HH.label
          [ HP.class_ (HH.ClassName "field__label")
          , HP.for fieldId
          ]
          [ HH.text label ]
      , HH.input
          ( [ HP.class_ (HH.ClassName "field__input")
            , HP.id fieldId
            , HP.type_ HP.InputText
            , HP.value value
            , HE.onValueInput onChange
            ] <> if autofocus then [ HP.autofocus true ] else []
          )
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

  makeNumberField
    :: { label :: String
       , fieldId :: String
       , value :: String
       , onChange :: String -> Action
       , autofocus :: Boolean
       }
    -> H.ComponentHTML Action () m
  makeNumberField { label, fieldId, value, onChange, autofocus } =
    HH.div
      [ HP.class_ (HH.ClassName "field") ]
      [ HH.label
          [ HP.class_ (HH.ClassName "field__label")
          , HP.for fieldId
          ]
          [ HH.text label ]
      , HH.input
          ( [ HP.class_ (HH.ClassName "field__input")
            , HP.id fieldId
            , HP.type_ HP.InputNumber
            , HP.step (HP.Step 0.01)
            , HP.min 0.0
            , HP.value value
            , HE.onValueInput onChange
            ] <> if autofocus then [ HP.autofocus true ] else []
          )
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

  renderContextMenu :: ContextMenuData -> H.ComponentHTML Action () m
  renderContextMenu { x, y, soundexDescr } =
    HH.div
      [ HP.class_ (HH.ClassName "context-menu")
      , HP.style $ "position: absolute; left: " <> show x <> "px; top: " <> show y <> "px; z-index: 1000;"
      ]
      [ HH.div
          [ HP.class_ (HH.ClassName "context-menu-debug") ]
          [ HH.text $ "SOUNDEX: " <> soundexDescr ]
      , HH.ul_
          [ HH.li
              [ HE.onClick \_ -> ContextMenuAction "Find similar transactions" ]
              [ HH.text "Find similar transactions" ]
          , HH.li
              [ HE.onClick \_ -> ContextMenuAction "Something else..." ]
              [ HH.text "Something else..." ]
          ]
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction action = case Debug.spy "action" action of
  -- handleAction = case _ of
  Initialize -> do
    -- Set up SSE subscription
    state <- H.get
    sseEmitter <- createSSEEmitter (unwrap state.apiBaseUrl)
    _ <- H.subscribe sseEmitter
    liftEffect $ log "[SSE] Subscription established"

    handleAction LoadAllData
  LoadAllData -> do
    H.modify_ \s -> s { data = Loading }
    state <- H.get
    result <- H.liftAff $ fetchAllData state.apiBaseUrl state.searchForm
    case result of
      Left err -> H.modify_ \s -> s { data = Failed err }
      Right appData -> do
        H.modify_ \s -> s { data = Loaded appData }
  LoadLedgerView -> do
    state <- H.get
    case state.data of
      Loaded appData -> do
        result <- H.liftAff $ fetchLedgerView state.apiBaseUrl state.searchForm
        case result of
          Left err -> H.modify_ \s -> s { data = Failed err }
          Right ledgerRows -> do
            H.modify_ \s -> s { data = Loaded (appData { ledgerRows = ledgerRows }) }
            -- Restore scroll position if we have one saved
            case state.scrollY of
              Just scrollY -> do
                H.liftEffect $ restoreScrollY scrollY
                H.modify_ \s -> s { scrollY = Nothing } -- Clear the saved position
              Nothing -> pure unit
      _ -> pure unit -- Only update if data is already loaded
  ToggleDarkMode -> do
    state <- H.get
    let newDarkMode = not state.isDarkMode
    H.modify_ \s -> s { isDarkMode = newDarkMode }
    liftEffect $ toggleTheme unit

  OpenCreateDialog -> do
    currentDateTime <- liftEffect getCurrentDateTimeLocal
    let
      initialState =
        { description: ""
        , fromAccountId: ""
        , toAccountId: ""
        , amount: ""
        , date: currentDateTime
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
                , description: row.descr
                , fromAccountId: show row.fromAccountId
                , toAccountId: show row.toAccountId
                , amount: stripNegativeSign row.flow
                , date: formatUnixToDateTimeLocal row.dateUnix
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
        -- Parse datetime-local to Unix timestamp
        let dateUnix = dateTimeLocalToUnix createState.date
        -- Convert amount string to cents
        case parseAmount createState.amount of
          Nothing -> do
            liftEffect $ log "Invalid amount format"
          Just cents -> do
            -- Create transaction request body
            let
              transactionData =
                { budgetId: (Nothing :: Maybe Int)
                , fromAccountId: parseAccountId createState.fromAccountId
                , toAccountId: parseAccountId createState.toAccountId
                , dateUnix: dateUnix
                , descr: createState.description
                , cents: cents
                }
            liftEffect $ log $ "Sending transaction: " <> JSON.writeJSON transactionData
            -- Send POST request
            result <- H.liftAff $ AX.request
              AX.defaultRequest
                { method = Left POST
                , url = unApiBaseUrl $ appendPath state.apiBaseUrl "/transactions"
                , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
                , content = Just $ RequestBody.string $ JSON.writeJSON transactionData
                , responseFormat = ResponseFormat.string
                }
            case result of
              Left err -> liftEffect $ log $ "Error creating transaction: " <> AX.printError err
              Right response -> do
                liftEffect $ log $ "Transaction created successfully. Status: " <> show response.status
                liftEffect $ dialogClose "transaction-dialog"
                H.modify_ \s -> s { dialog = Nothing }
                -- Refresh all data
                handleAction LoadAllData
      Just (EditDialog editState) -> do
        -- Parse datetime-local to Unix timestamp
        let dateUnix = dateTimeLocalToUnix editState.date
        -- Convert amount string to cents
        case parseAmount editState.amount of
          Nothing -> do
            liftEffect $ log "Invalid amount format"
          Just cents -> do
            -- Create transaction update request body
            let
              transactionData =
                { budgetId: (Nothing :: Maybe Int)
                , fromAccountId: parseAccountId editState.fromAccountId
                , toAccountId: parseAccountId editState.toAccountId
                , dateUnix: dateUnix
                , descr: editState.description
                , cents: cents
                }
            liftEffect $ log $ "Updating transaction " <> show editState.transactionId <> ": " <> JSON.writeJSON transactionData
            -- Send PUT request
            result <- H.liftAff $ AX.request
              AX.defaultRequest
                { method = Left PUT
                , url = unApiBaseUrl $ appendPath state.apiBaseUrl ("/transactions/" <> show editState.transactionId)
                , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
                , content = Just $ RequestBody.string $ JSON.writeJSON transactionData
                , responseFormat = ResponseFormat.string
                }
            case result of
              Left err -> liftEffect $ log $ "Error updating transaction: " <> AX.printError err
              Right response -> do
                liftEffect $ log $ "Transaction updated successfully. Status: " <> show response.status
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

  DeleteTransaction -> do
    state <- H.get
    case state.dialog of
      Just (EditDialog editState) -> do
        confirmed <- liftEffect $ confirmDialog "Are you sure you want to delete this transaction?"
        when confirmed do
          -- Send DELETE request
          result <- H.liftAff $ AX.request
            AX.defaultRequest
              { method = Left DELETE
              , url = unApiBaseUrl $ appendPath state.apiBaseUrl ("/transactions/" <> show editState.transactionId)
              , responseFormat = ResponseFormat.string
              }
          case result of
            Left err -> do
              liftEffect $ log $ "Error deleting transaction: " <> AX.printError err
            -- TODO: Show error in toast notification when implemented
            Right response -> do
              liftEffect $ log $ "Transaction deleted successfully. Status: " <> show response.status
              liftEffect $ dialogClose "transaction-dialog"
              H.modify_ \s -> s { dialog = Nothing }
              -- Refresh all data
              handleAction LoadAllData
      _ -> pure unit

  FilterChanged filterAction -> do
    state <- H.get
    let newSearchForm = updateSearchForm filterAction state.searchForm
    H.modify_ \s -> s { searchForm = newSearchForm }

    -- Cancel existing timer if any
    case state.debounceTimerId of
      Nothing -> pure unit
      Just timerId -> H.kill timerId

    -- Set up new debounce timer
    timerId <- H.fork do
      H.liftAff $ delay (Milliseconds 200.0)
      handleAction DebouncedFilterUpdate

    -- Store the new timer ID
    H.modify_ \s -> s { debounceTimerId = Just timerId }

  DebouncedFilterUpdate -> do
    -- Clear the timer ID since it fired
    H.modify_ \s -> s { debounceTimerId = Nothing }
    -- Now actually load the filtered data
    handleAction LoadLedgerView

  ShowContextMenu evt { soundexDescr, descr } -> do
    H.liftEffect $ Event.preventDefault evt
    case MouseEvent.fromEvent evt of
      Just mouseEvent -> do
        let x = toNumber $ MouseEvent.pageX mouseEvent
        let y = toNumber $ MouseEvent.pageY mouseEvent
        let contextMenuData = { transactionId: 0, x, y, soundexDescr, descr }
        H.liftEffect $ Console.log $ "ShowContextMenu: " <> show { x, y }
        H.modify_ \s -> s { contextMenu = Just contextMenuData }
      Nothing -> do
        pure unit

  HideContextMenu -> do
    H.modify_ \s -> s { contextMenu = Nothing }

  ContextMenuAction menuAction -> do
    state <- H.get
    case menuAction of
      "Find similar transactions" -> do
        -- Get the soundexDescr from the context menu data
        case state.contextMenu of
          Just contextMenuData -> do
            -- Update the filter state with the soundexDescr
            let newSearchForm = state.searchForm { searchBy = BySimilarTo contextMenuData }
            H.modify_ \s -> s
              { searchForm = newSearchForm
              , contextMenu = Nothing
              }
            -- Trigger the filtered data load
            handleAction LoadLedgerView
          Nothing ->
            H.modify_ \s -> s { contextMenu = Nothing }
      _ -> do
        -- For other actions, just hide the context menu for now
        H.modify_ \s -> s { contextMenu = Nothing }

  ApplySuggestionItem event { transaction, accountId } -> do
    -- Prevent the event from propagating to the transaction row (which would open the edit dialog)
    H.liftEffect $ Event.preventDefault event
    H.liftEffect $ Event.stopPropagation event

    -- Capture current scroll position before applying suggestion
    scrollY <- H.liftEffect getScrollY
    H.modify_ \s -> s { scrollY = Just scrollY }

    -- Apply the suggestion by updating the transaction's toAccountId
    state <- H.get
    let
      transactionData =
        { budgetId: (Nothing :: Maybe Int)
        , fromAccountId: transaction.fromAccountId
        , toAccountId: accountId -- This is the suggested account ID
        , dateUnix: transaction.dateUnix
        , descr: transaction.descr
        , cents: if transaction.flowCents < 0 then (-transaction.flowCents) else transaction.flowCents -- Ensure positive value
        }
    liftEffect $ log $ "Applying suggestion: updating transaction " <> show transaction.transactionId <> " to account " <> show accountId

    -- Send PUT request to update the transaction
    result <- H.liftAff $ AX.request
      AX.defaultRequest
        { method = Left PUT
        , url = unApiBaseUrl $ appendPath state.apiBaseUrl ("/transactions/" <> show transaction.transactionId)
        , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
        , content = Just $ RequestBody.string $ JSON.writeJSON transactionData
        , responseFormat = ResponseFormat.string
        }
    case result of
      Left err -> liftEffect $ log $ "Error applying suggestion: " <> AX.printError err
      Right response -> do
        liftEffect $ log $ "Suggestion applied successfully. Status: " <> show response.status
        -- Refresh only the ledger view to show the updated transaction
        handleAction LoadLedgerView

  ApplyAllSuggestions suggestions -> do
    -- Apply all suggestions using the batch endpoint
    state <- H.get

    if Array.length suggestions == 0 then do
      liftEffect $ log "No suggestions to apply"
    else do
      liftEffect $ log $ "Applying " <> show (Array.length suggestions) <> " suggestions in batch"

      -- Convert to the format expected by batchApplySuggestions
      let suggestionInserts = map (\s -> { transactionId: s.transactionId, toAccountId: s.accountId }) suggestions

      -- Call the batch suggestions endpoint
      result <- H.liftAff $ AX.request
        AX.defaultRequest
          { method = Left POST
          , url = unApiBaseUrl $ appendPath state.apiBaseUrl "/transactions/suggestions"
          , headers = [ RequestHeader.RequestHeader "Content-Type" "application/json" ]
          , content = Just $ RequestBody.string $ JSON.writeJSON suggestionInserts
          , responseFormat = ResponseFormat.string
          }

      case result of
        Right response -> do
          liftEffect $ log $ "All suggestions applied successfully. Status: " <> show response.status
          -- Refresh the ledger view to show the updated transactions
          handleAction LoadLedgerView
        Left err -> do
          liftEffect $ log $ "Failed to apply batch suggestions: " <> AX.printError err

  GotSSE_Message message -> do
    H.modify_ \s -> s { last_SSE_Message = Just message }

findTransaction :: Int -> Array LedgerViewRow -> Maybe LedgerViewRow
findTransaction transactionId rows =
  case Array.uncons rows of
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

updateSearchForm :: FilterAction -> SearchForm -> SearchForm
updateSearchForm action state =
  case action of
    FilterDescriptionChanged desc -> state { searchBy = ByDescr desc }
    FilterMinAmountChanged minAmount -> state { minAmount = minAmount }
    FilterMaxAmountChanged maxAmount -> state { maxAmount = maxAmount }
    FilterUnknownExpensesToggled -> state { filterUnknownExpenses = not state.filterUnknownExpenses }
    ClearFilters ->
      { searchBy: ByDescr ""
      , minAmount: ""
      , maxAmount: ""
      , filterUnknownExpenses: false
      }

-- baseUrl is now passed as apiBaseUrl in state

fetchLedgerView :: ApiBaseUrl -> SearchForm -> Aff (Either String (Array LedgerViewRow))
fetchLedgerView apiBaseUrl searchForm = do
  -- Build query parameters from search form
  let queryParams = buildQueryParams searchForm
  let url = unApiBaseUrl $ appendPath apiBaseUrl ("/accounts/2/ledger" <> queryParams)
  -- Fetch ledger view for checking account (ID = 2) from the API
  result <- AX.get ResponseFormat.string url
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (ledgerRows :: Array LedgerViewRow) ->
          pure $ Right ledgerRows

fetchAccounts :: ApiBaseUrl -> Aff (Either String (Array Account))
fetchAccounts apiBaseUrl = do
  -- Fetch accounts from the API
  result <- AX.get ResponseFormat.string (unApiBaseUrl $ appendPath apiBaseUrl "/accounts")
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (accounts :: Array Account) ->
          pure $ Right accounts

fetchBalances :: ApiBaseUrl -> Aff (Either String (Array AccountBalanceRead))
fetchBalances apiBaseUrl = do
  -- Fetch account balances from the API
  -- Using accountIds=2,3 to match the Elm implementation
  result <- AX.get ResponseFormat.string (unApiBaseUrl $ appendPath apiBaseUrl "/accounts/balances?accountIds=2,3")
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (balances :: Array AccountBalanceRead) ->
          pure $ Right balances

fetchSuggestions :: ApiBaseUrl -> Aff (Either String (Array Suggestion))
fetchSuggestions apiBaseUrl = do
  -- Fetch suggestions from the API
  -- Use typical account IDs: fromAccountId=2 (checking), toAccountId=6 (unknown expenses)
  result <- AX.get ResponseFormat.string (unApiBaseUrl $ appendPath apiBaseUrl "/transactions/suggestions?fromAccountId=2&toAccountId=6")
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right (suggestions :: Array Suggestion) ->
          pure $ Right suggestions

-- Foreign import to call JavaScript theme toggle
foreign import toggleTheme :: Unit -> Effect Unit

-- Foreign imports for dialog control
foreign import dialogShow :: String -> Effect Unit
foreign import dialogClose :: String -> Effect Unit

-- Foreign import for date formatting
foreign import formatUnixToDateTimeLocal :: Int -> String

-- Foreign import for native confirmation dialog
foreign import confirmDialog :: String -> Effect Boolean

-- Foreign import for getting current datetime
foreign import getCurrentDateTimeLocal :: Effect String

-- Foreign imports for scroll position management
foreign import getScrollY :: Effect Number
foreign import restoreScrollY :: Number -> Effect Unit

type InitArgs = { isDarkMode :: Boolean, apiBaseUrl :: String }

main :: InitArgs -> Effect Unit
main initArgs = do
  log "[CLIENT/PS] Booting up..."
  log $ "[CLIENT/PS] API Base URL: " <> initArgs.apiBaseUrl
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component initArgs body

-- | Create an SSE subscription emitter
createSSEEmitter :: forall m. MonadAff m => String -> m (HS.Emitter Action)
createSSEEmitter apiBaseUrl = do
  { emitter, listener } <- H.liftEffect HS.create

  -- Set up SSE connection and notify listener with actions
  _ <- H.liftEffect $ setupSSEConnection apiBaseUrl \message -> do
    HS.notify listener (GotSSE_Message message)

  pure emitter

-- | Set up EventSource connection from PureScript
foreign import setupSSEConnection :: String -> (String -> Effect Unit) -> Effect Unit

-- Helper functions for transaction creation

-- Convert datetime-local string (YYYY-MM-DDTHH:mm) to Unix timestamp
dateTimeLocalToUnix :: String -> Int
dateTimeLocalToUnix dateTimeStr =
  -- Use the foreign function for parsing
  parseDateTimeLocal dateTimeStr

-- Foreign import for parsing datetime-local
foreign import parseDateTimeLocal :: String -> Int

-- Parse amount string (e.g., "42.50") to cents (always positive)
parseAmount :: String -> Maybe Int
parseAmount amountStr = do
  amount <- Number.fromString amountStr
  pure $ Int.round (Number.abs amount * 100.0)

-- Parse account ID string to Int
parseAccountId :: String -> Int
parseAccountId idStr = fromMaybe 0 (Int.fromString idStr)

-- Strip negative sign from amount string to ensure positive values in dialogs
stripNegativeSign :: String -> String
stripNegativeSign amountStr =
  if String.take 1 amountStr == "-" then String.drop 1 amountStr
  else amountStr

-- Convert euro amount string to cents integer (e.g., "40.50" -> 4050)
eurosToCents :: String -> Maybe Int
eurosToCents euroStr = do
  euros <- Number.fromString euroStr
  pure $ Int.round $ euros * 100.0

-- Build query parameters from search form
buildQueryParams :: SearchForm -> String
buildQueryParams fsUntrimmed =
  let
    fs = fsUntrimmed
      { minAmount = String.trim fsUntrimmed.minAmount
      , maxAmount = String.trim fsUntrimmed.maxAmount
      , filterUnknownExpenses = fsUntrimmed.filterUnknownExpenses
      }
    whenStr str encoded = if str /= "" then Just encoded else Nothing
    whenBool bool encoded = if bool then Just encoded else Nothing

    -- Convert euro amounts to cents for API
    minAmountCents = if fs.minAmount == "" then Nothing else eurosToCents fs.minAmount
    maxAmountCents = if fs.maxAmount == "" then Nothing else eurosToCents fs.maxAmount

    -- Flatten SearchBy into query parameters for backend
    descriptionParam = case fs.searchBy of
      ByDescr desc -> whenStr (String.trim desc) ("description=" <> String.trim desc)
      BySimilarTo _ -> Nothing

    soundexParam = case fs.searchBy of
      ByDescr _ -> Nothing
      BySimilarTo contextMenuData -> whenStr (String.trim contextMenuData.soundexDescr) ("soundexDescr=" <> String.trim contextMenuData.soundexDescr)

    params =
      Array.catMaybes
        [ descriptionParam
        , soundexParam
        , map (\cents -> "minAmount=" <> show cents) minAmountCents
        , map (\cents -> "maxAmount=" <> show cents) maxAmountCents
        , whenBool fs.filterUnknownExpenses "filterUnknownExpenses=1"
        ]
  in
    case params of
      [] -> ""
      _ -> "?" <> String.joinWith "&" params
