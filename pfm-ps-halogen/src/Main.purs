-- ( Dialog1FormMsg(..) -- , main -- ) where
module Main where

import Effect.Now
import Prelude
import Data.Array (cons, foldMap, nubBy)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Date (Date)
import Data.DateTime (DateTime(..), adjust)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.DateTime.Instant as Instant
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Foldable (elem, fold, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String as String
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..), fst)
import Debug (spy)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
import Effect.Now (now, nowDateTime)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)

foreign import unsafeStringify :: forall a. a -> String

foreign import dialogShow :: String -> Effect Unit

foreign import dialogClose :: String -> Effect Unit

foreign import padRight :: Int -> Char -> String -> String

myDialog1Id :: String
myDialog1Id = "myDialog1"

myDialog2Id :: String
myDialog2Id = "myDialog2"

main :: Effect Unit
main = do
  refDate <- daysAgo 7
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (component refDate) unit body

type Person
  = { name :: String
    , age :: Int
    }

type Dialog1
  = { input1 :: String
    , select1 :: String
    , input2 :: String
    , select2 :: String
    }

emptyForm1 :: Dialog1
emptyForm1 =
  { input1: ""
  , select1: ""
  , input2: ""
  , select2: ""
  }

type Dialog2
  = { name :: String
    , email :: String
    , comment :: String
    }

emptyForm2 :: Dialog2
emptyForm2 =
  { name: ""
  , email: ""
  , comment: ""
  }

data Dialog
  = MkDialog1 Dialog1
  | MkDialog2 Dialog2

type Category
  = { name :: String
    }

type Account
  = { name :: String
    , category :: Category
    }

type TransactionView
  = { date :: Instant
    , descr :: String
    , from :: Account
    , to :: Account
    , amount :: Decimal
    }

type State
  = { dialog :: Maybe Dialog
    , book :: Map Int TransactionView
    }

-- derive instance Generic State _
-- instance Show State where
--   show = genericShow
data Dialog1FormMsg
  = Input1Changed String
  | Select1Changed String
  | Input2Changed String
  | Select2Changed String

data Dialog2FormMsg
  = NameChanged String
  | EmailChanged String
  | CommentChanged String

data Action
  = ShowDialog1
  | ShowDialog2
  | CloseDialog { dialogId :: String }
  | Dialog1FormMsg Dialog1FormMsg
  | Dialog2FormMsg Dialog2FormMsg

daysAgo :: Int -> Effect Instant
daysAgo d = do
  x <- DateTime.adjust (Duration.Hours $ (Int.toNumber d) * -24.0) <$> nowDateTime
  case x of
    Nothing -> throw "Invalid date" -- This can't happen. Int will overflow first
    Just i -> pure (fromDateTime i)

oneWeekAgo :: Effect DateTime
oneWeekAgo = do
  now <- nowDateTime
  pure $ fromMaybe now $ DateTime.adjust (Duration.Days (-7.0)) now

subtractDay :: Instant -> Instant
subtractDay instant =
  let
    negativeDuration = Duration.negateDuration (Duration.Hours 24.0)
  in
    fromMaybe instant $ fromDateTime <$> adjust negativeDuration (toDateTime instant)

instantOrThrow :: Number -> Effect Instant
instantOrThrow ms = case Instant.instant (Duration.Milliseconds ms) of
  Just i -> pure i
  Nothing -> throw $ "Invalid timestamp: " <> show ms <> " is outside the valid range for Instant"

subtractDay' :: Instant -> Instant
subtractDay' instant =
  let
    negativeDuration = Duration.negateDuration (Duration.Hours 24.0)
  in
    fromDateTime $ unsafePartial $ fromJust $ adjust negativeDuration (toDateTime instant)

zero :: Maybe Instant
zero = Instant.instant (Duration.Milliseconds 0.0)

subtractDay3 :: Instant -> Maybe Instant
subtractDay3 inst = Instant.instant $ Instant.unInstant inst

-- 160 is the Unicode code point for non-breaking space
nbsp :: Char
nbsp = unsafePartial $ fromJust $ (fromCharCode 160)

oneDayAgo :: Effect Instant
oneDayAgo = do
  now' <- now
  pure $ subtractDay now'

equity :: Category
equity = { name: "Equity" }

assets :: Category
assets = { name: "Assets" }

checkingAccount :: Account
checkingAccount =
  { name: "Checking account"
  , category: assets
  }

savingsAccount :: Account
savingsAccount =
  { name: "Savings account"
  , category: assets
  }

employerABC :: Account
employerABC =
  { name: "EmployerABC"
  , category: income
  }

customerXYZ :: Account
customerXYZ =
  { name: "CustomerXYZ"
  , category: income
  }

income :: Category
income = { name: "Income" }

openingBalance :: Account
openingBalance =
  { name: "OpeningBalance"
  , category: equity
  }

expenses :: Category
expenses = { name: "Expenses" }

spar :: Account
spar =
  { name: "Spar"
  , category: expenses
  }

amazon :: Account
amazon =
  { name: "Amazon"
  , category: expenses
  }

balance :: Map Int TransactionView -> Account -> Decimal
balance txs account =
  foldlWithIndex
    ( \_ total ({ from, to, amount }) ->
        if to == account then
          total + amount
        else if from == account then
          total - amount
        else
          total
    )
    (Decimal.fromInt 0)
    txs

amountFmt :: Decimal -> Tuple String String
amountFmt amount =
  let
    str = case Decimal.toString amount # split (Pattern ".") of
      [ euros, cents ] -> euros <> "." <> padRight 2 '0' cents
      [ euros ] -> euros <> ".00"
      _ -> "IMPOSSIBLE"
  in
    Tuple str "€"

allAccounts_ :: Array Account
allAccounts_ =
  [ checkingAccount
  , savingsAccount
  , openingBalance
  , employerABC
  , customerXYZ
  , spar
  , amazon
  ]

allAccounts :: Map String Account
allAccounts =
  allAccounts_
    # map (\o -> Tuple o.name o)
    # Map.fromFoldable

-- uniqueBy :: forall a b. (a -> b) -> Array a -> Array a
-- uniqueBy toComparable =
--   fst
--     <<< Array.foldl
--       ( \item (Tuple items keys) ->
--           if elem (toComparable item) keys then
--             (Tuple items keys)
--           else
--             ( Tuple
--                 (cons item items)
--                 (Set.insert (toComparable item) keys)
--             )
--       )
--       (Tuple [] Set.empty)
component :: forall query output m. MonadEffect m => Instant -> H.Component query Unit output m
component refTS =
  H.mkComponent
    { initialState
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              }
    , render
    }
  where
  onDay :: Int -> Instant
  onDay d =
    fromDateTime
      $ unsafePartial
      $ fromJust
      $ DateTime.adjust
          (Duration.Hours $ (Int.toNumber d) * -24.0)
          (toDateTime refTS)

  initialState :: Unit -> State
  initialState _ =
    { dialog: Nothing
    , book:
        Map.fromFoldable
          [ Tuple
              1
              { date: onDay 0
              , descr: "Opening balance"
              , from: openingBalance
              , to: checkingAccount
              , amount: Decimal.fromNumber 1000.00
              }
          , Tuple
              2
              { date: onDay 1
              , descr: "Groceries"
              , from: checkingAccount
              , to: spar
              , amount: Decimal.fromNumber 9.99
              }
          , Tuple
              3
              { date: onDay 2
              , descr: "Book purchase"
              , from: checkingAccount
              , to: amazon
              , amount: Decimal.fromNumber 54.99
              }
          , Tuple
              4
              { date: onDay 3
              , descr: "Groceries, again"
              , from: checkingAccount
              , to: spar
              , amount: Decimal.fromNumber 37.42
              }
          , Tuple
              5
              { date: onDay 4
              , descr: "Salary"
              , from: employerABC
              , to: checkingAccount
              , amount: Decimal.fromNumber 100.00
              }
          ]
    }

  handleDialog1FormMsg :: Dialog1FormMsg -> Dialog1 -> Dialog1
  handleDialog1FormMsg msg form = case msg of
    Input1Changed value -> form { input1 = value }
    Select1Changed value -> form { select1 = value }
    Input2Changed value -> form { input2 = value }
    Select2Changed value -> form { select2 = value }

  handleDialog2FormMsg :: Dialog2FormMsg -> Dialog2 -> Dialog2
  handleDialog2FormMsg msg form = case msg of
    NameChanged value -> form { name = value }
    EmailChanged value -> form { email = value }
    CommentChanged value -> form { comment = value }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ShowDialog1 -> do
      H.modify_ \state -> state { dialog = Just (MkDialog1 emptyForm1) }
      H.liftEffect $ dialogShow myDialog1Id
    ShowDialog2 -> do
      H.modify_ \state -> state { dialog = Just (MkDialog2 emptyForm2) }
      H.liftEffect $ dialogShow myDialog2Id
    CloseDialog { dialogId } -> do
      H.liftEffect $ dialogClose dialogId
      H.modify_ \state -> state { dialog = Nothing }
    Dialog1FormMsg msg ->
      H.modify_ \state ->
        state
          { dialog =
            case state.dialog of
              Just (MkDialog1 form) -> Just (MkDialog1 (handleDialog1FormMsg msg form))
              _ -> state.dialog
          }
    Dialog2FormMsg msg ->
      H.modify_ \state ->
        state
          { dialog =
            case state.dialog of
              Just (MkDialog2 form) -> Just (MkDialog2 (handleDialog2FormMsg msg form))
              _ -> state.dialog
          }

  render :: State -> H.ComponentHTML Action () m
  render state =
    -- let
    --   _ = spy "render" state
    -- in
    let
      allCategories :: Array Category
      allCategories =
        state.book
          # Map.values
          # Array.fromFoldable
          # Array.concatMap (\tx -> [ tx.from.category, tx.to.category ])
          # nubBy (\a b -> compare a.name b.name)

      transactions :: Array (Tuple Int TransactionView)
      transactions =  -- Array.sortBy
        --   (\(Tuple _ tx) -> tx.date)
        --   (Map.toUnfoldable state.book)
        Array.sortBy
          (comparing (\(Tuple _ tx) -> tx.date))
          (Map.toUnfoldable state.book)
    --
    --
    --
    -- transactionsWithBalance : List ( Tuple Int TransactionViewWithBalance )
    -- transactionsWithBalance =
    --           let
    --               f ::
    --                   ( Tuple Int TransactionView )
    --                   -> ( Decimal, List (Tuple Int TransactionViewWithBalance ) )
    --                   -> ( Decimal, List (Tuple Int TransactionViewWithBalance ) )
    --               f ( Tuple transactionId o ) (Tuple prevBalance txs ) =
    --                   let
    --                       newBalance =
    --                           Decimal.add
    --                               prevBalance
    --                               (if o.to.name == checkingAccount.name then
    --                                   o.amount
    --                                else if o.from.name == checkingAccount.name then
    --                                   Decimal.negate o.amount
    --                                else
    --                                   zero
    --                               )
    --                   in
    --                   ( Tuple newBalance
    --                    (cons ( Tuple transactionId
    --                      { date = o.date
    --                       , descr = o.descr
    --                       , from = o.from
    --                       , to = o.to
    --                       , amount = o.amount
    --                       , balanceMovement = { from = prevBalance, to = newBalance }
    --                       }
    --                     ) txs)
    --                   )
    --           in
    --           List.reverse $
    --               Tuple.second $
    --                   List.foldl
    --                       f
    --                       ( zero, [] )
    --                       transactions
    --
    --
    --
    --
    in
      HH.div
        [ HP.class_ $ HH.ClassName "container" ]
        [ HH.div [ HP.class_ $ HH.ClassName "section" ]
            [ HH.div [ HP.class_ $ HH.ClassName "debug-info" ]
                [ HH.div [ HP.class_ $ HH.ClassName "section" ]
                    [ HH.h2 [ HP.class_ $ HH.ClassName "section-title" ] [ HH.text "Balances" ]
                    , HH.div [ HP.class_ $ HH.ClassName "balances" ]
                        ( Array.concatMap
                            ( \category ->
                                map
                                  ( \account ->
                                      let
                                        accountBalance = balance state.book account

                                        colorAccent =
                                          if category.name == "Assets" then
                                            "#3498db"
                                          else if category.name == "Expenses" then
                                            "#e74c3c"
                                          else
                                            "#9b59b6"
                                      in
                                        HH.div
                                          [ HP.class_ $ HH.ClassName "balance-card"
                                          , HP.style $ "border-left-color:" <> colorAccent
                                          ]
                                          [ HH.div
                                              [ HP.class_ $ HH.ClassName "balance-card__category" ]
                                              [ HH.text category.name ]
                                          , HH.div
                                              [ HP.class_ $ HH.ClassName "balance-card__account" ]
                                              [ HH.text account.name ]
                                          , HH.div
                                              [ HP.class_ $ HH.ClassName "balance-card__amount" ]
                                              ( (amountFmt accountBalance)
                                                  # \(Tuple amountStr currency) ->
                                                      [ HH.text amountStr
                                                      , HH.text $ String.singleton $ String.codePointFromChar nbsp
                                                      , HH.text currency
                                                      ]
                                              )
                                          ]
                                  )
                                  (Array.filter (\o -> o.category == category) allAccounts_)
                            )
                            ( Array.sortBy (comparing (\c -> c.name))
                                $ Array.filter
                                    ( \c ->
                                        elem c
                                          [ assets
                                          -- , expenses
                                          -- , income
                                          ]
                                    )
                                    allCategories
                            )
                        )
                    ]
                --     , H.div [ HA.class "section" ]
                --         [ H.div [ HA.class "transaction-list" ]
                --             [ H.div [ HA.class "transaction-list__header" ]
                --                 [ H.h3 [] [ H.text "Transactions" ]
                --                 , H.button
                --                     [ HA.class "button button--primary"
                --                     , HE.onClick AddTransactionClicked
                --                     ]
                --                     [ H.text "Add Transaction" ]
                --                 ]
                --             , H.ul [ HA.class "transaction-list__items" ]
                --                 (List.map
                --                     (\( transactionId, tx ) ->
                --                         let
                --                             isPositive =
                --                                 Decimal.gt tx.amount Decimal.zero && tx.from /= checkingAccount
                --                             amountClass =
                --                                 if isPositive then
                --                                     "transaction-item__amount transaction-item__amount--positive"
                --                                 else
                --                                     "transaction-item__amount transaction-item__amount--negative"
                --                             amountSign =
                --                                 if isPositive then
                --                                     "+"
                --                                 else
                --                                     "-"
                --                         in
                --                         H.li
                --                             [ HA.class "transaction-item"
                --                             , HE.onClick (EditTransactionClicked transactionId)
                --                             ]
                --                             [ H.div [ HA.class "transaction-item__row" ]
                --                                 [ H.div [ HA.class "transaction-item__main-content" ]
                --                                     [ H.div [ HA.class "transaction-item__details" ]
                --                                         [ H.div [ HA.class "transaction-item__description" ]
                --                                             [ H.text tx.descr ]
                --                                         , H.div [ HA.class "transaction-item__accounts" ]
                --                                             [ H.text (tx.from.name ++ " → " ++ tx.to.name) ]
                --                                         ]
                --                                     , H.div [ HA.class "transaction-item__date" ]
                --                                         [ H.text (dateFmt tx.date) ]
                --                                     , H.div [ HA.class amountClass ]
                --                                         [ H.text (amountSign ++ amountFmt tx.amount) ]
                --                                     ]
                --                                 , H.div [ HA.class "transaction-item__balance-column" ]
                --                                     [ H.div [ HA.class "transaction-item__balance-movement" ]
                --                                         [ H.span [ HA.class "balance-before" ] [ H.text (amountFmt tx.balanceMovement.from) ]
                --                                         , H.span [ HA.class "arrow-icon" ] [ H.text " → " ]
                --                                         , H.span [ HA.class "balance-after" ] [ H.text (amountFmt tx.balanceMovement.to) ]
                --                                         ]
                --                                     ]
                --                                 ]
                --                             ]
                --                     )
                --                     transactionsWithBalance
                --                 )
                --             ]
                --         ]
                --     , case model.dialog of
                --         Nothing ->
                --             H.text ""
                --         Just (EditDialog data) ->
                --             viewEditDialog data
                --         Just (CreateDialog data) ->
                --             viewCreateDialog data
                --     ]
                --         , HH.div [ HP.style "margin: 10px 0" ]
                --             [ HH.button
                --                 [ HE.onClick $ const ShowDialog1
                --                 , HP.style "margin-right: 10px"
                --                 ]
                --                 [ HH.text "Open Dialog 1" ]
                --             , HH.button
                --                 [ HE.onClick $ const ShowDialog2 ]
                --                 [ HH.text "Open Dialog 2" ]
                --             ]
                --         , case state.dialog of
                --             Just (MkDialog1 form) -> viewDialog1 form
                --             Just (MkDialog2 form) -> viewDialog2 form
                --             Nothing -> HH.text ""
                --         ]
                --     ]
                -- ]
                ]
            ]
        ]

  viewDialog1 :: Dialog1 -> H.ComponentHTML Action () m
  viewDialog1 form =
    HH.dialog [ HP.id myDialog1Id ]
      [ HH.div [ HP.class_ $ HH.ClassName "dialog-content" ]
          [ HH.h3 [] [ HH.text "Dialog 1 Form" ]
          , HH.label [ HP.for "input1" ] [ HH.text "Input 1:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id "input1"
              , HE.onValueInput (Dialog1FormMsg <<< Input1Changed)
              , HP.value form.input1
              ]
          , HH.label [ HP.for "select1" ] [ HH.text "Select 1:" ]
          , HH.select [ HP.id "select1", HE.onValueInput (Dialog1FormMsg <<< Select1Changed) ]
              [ HH.option [ HP.value "option1", HP.selected (form.select1 == "option1") ] [ HH.text "Option 1" ]
              , HH.option [ HP.value "option2", HP.selected (form.select1 == "option2") ] [ HH.text "Option 2" ]
              ]
          , HH.label [ HP.for "input2" ] [ HH.text "Input 2 (Target Focus):" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id "input2"
              , HE.onValueInput (Dialog1FormMsg <<< Input2Changed)
              , HP.value form.input2
              , HP.autofocus true
              ]
          , HH.label [ HP.for "select2" ] [ HH.text "Select 2:" ]
          , HH.select [ HP.id "select2", HE.onValueInput (Dialog1FormMsg <<< Select2Changed) ]
              [ HH.option [ HP.value "option3", HP.selected (form.select2 == "option3") ] [ HH.text "Option 3" ]
              , HH.option [ HP.value "option4", HP.selected (form.select2 == "option4") ] [ HH.text "Option 4" ]
              ]
          , HH.button
              [ HP.id "closeDialog"
              , HE.onClick $ const $ CloseDialog { dialogId: myDialog1Id }
              , HP.style "margin-top: 10px"
              ]
              [ HH.text "Close" ]
          ]
      ]

  viewDialog2 :: Dialog2 -> H.ComponentHTML Action () m
  viewDialog2 form =
    HH.dialog [ HP.id myDialog2Id ]
      [ HH.div [ HP.class_ $ HH.ClassName "dialog-content" ]
          [ HH.h3 [] [ HH.text "Dialog 2 - Feedback Form" ]
          , HH.label [ HP.for "name" ] [ HH.text "Name:" ]
          , Dialog2FormMsg
              <$> HH.input
                  [ HP.type_ HP.InputText
                  , HP.id "name"
                  , HE.onValueInput NameChanged
                  , HP.value form.name
                  ]
          , HH.label [ HP.for "email" ] [ HH.text "Email:" ]
          , Dialog2FormMsg
              <$> HH.input
                  [ HP.type_ HP.InputEmail
                  , HP.id "email"
                  , HE.onValueInput EmailChanged
                  , HP.value form.email
                  -- , HP.autofocus true
                  ]
          , HH.label [ HP.for "comment" ] [ HH.text "Comment:" ]
          , Dialog2FormMsg
              <$> HH.textarea
                  [ HP.id "comment"
                  , HE.onValueInput CommentChanged
                  , HP.value form.comment
                  , HP.style "width: 100%"
                  , HP.style "min-height: 100px"
                  ]
          , HH.br_
          , HH.button
              [ HP.id "closeDialog"
              , HE.onClick $ const $ CloseDialog { dialogId: myDialog2Id }
              , HP.style "margin-top: 10px"
              ]
              [ HH.text "Close" ]
          ]
      ]

  viewTable :: Array Person -> H.ComponentHTML Action () m
  viewTable people =
    HH.table [ HP.class_ $ HH.ClassName "table" ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "name" ]
              , HH.th_ [ HH.text "age" ]
              ]
          ]
      , HH.tbody_ $ map viewPerson people
      ]
    where
    viewPerson :: Person -> H.ComponentHTML Action () m
    viewPerson person =
      HH.tr_
        [ HH.td_ [ HH.text person.name ]
        , HH.td_ [ HH.text $ show person.age ]
        ]
