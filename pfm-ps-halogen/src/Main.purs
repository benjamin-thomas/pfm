-- ( Dialog1FormMsg(..) -- , main -- ) where
module Main where

import Effect.Now (now, nowDateTime)
import Prelude
import Data.Array (cons, nubBy)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.DateTime (DateTime, adjust)
import Data.DateTime as DateTime
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.DateTime.Instant as Instant
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Foldable (elem)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.Time.Duration as Duration
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
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

foreign import dateFmt :: Instant -> String

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

emptyCreateDialog :: Instant -> MkCreateDialog
emptyCreateDialog inst =
  { descr: ""
  , from: ""
  , to: ""
  , amount: ""
  , date: dateFmt inst
  , showTime: false
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

type MkEditDialog
  = { transactionId :: Int
    , descr :: String
    , from :: String
    , to :: String
    , amount :: String
    , date :: String
    , showTime :: Boolean
    }

type MkCreateDialog
  = { descr :: String
    , from :: String
    , to :: String
    , amount :: String
    , date :: String
    , showTime :: Boolean
    }

data Dialog
  = EditDialog MkEditDialog
  | CreateDialog MkCreateDialog

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

type TransactionViewWithBalance
  = { date :: Instant
    , descr :: String
    , from :: Account
    , to :: Account
    , amount :: Decimal
    , balanceMovement :: { from :: Decimal, to :: Decimal }
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
  = CloseDialog { dialogId :: String }
  | AddTransactionClicked
  | EditTransactionClicked Int

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

amountFmt :: Decimal -> String
amountFmt amount =
  let
    str = case Decimal.toString amount # split (Pattern ".") of
      [ euros, cents ] -> euros <> "." <> padRight 2 '0' cents
      [ euros ] -> euros <> ".00"
      _ -> "IMPOSSIBLE"
  in
    str <> (String.singleton $ String.codePointFromChar nbsp) <> "€"

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

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    AddTransactionClicked -> do
      n <- H.liftEffect now
      H.modify_ \state -> state { dialog = Just (CreateDialog $ emptyCreateDialog n) }
      H.liftEffect $ dialogShow myDialog1Id
    EditTransactionClicked transactionId -> do
      state <- H.get
      case Map.lookup transactionId state.book of
        Nothing -> H.liftEffect $ throw $ "Could not find transaction with id: " <> show transactionId
        Just tx ->
          H.modify_ \st ->
            st
              { dialog =
                Just
                  ( EditDialog
                      ( { transactionId: transactionId
                        , descr: tx.descr
                        , from: tx.from.name
                        , to: tx.to.name
                        , amount: Decimal.toString tx.amount
                        , date: dateFmt tx.date
                        , showTime: false
                        }
                      )
                  )
              }
      H.liftEffect $ dialogShow myDialog2Id
    CloseDialog { dialogId } -> do
      H.liftEffect $ dialogClose dialogId
      H.modify_ \state -> state { dialog = Nothing }

  -- Dialog1FormMsg msg ->
  --   H.modify_ \state ->
  --     state
  --       { dialog =
  --           case state.dialog of
  --             Just (MkDialog1 form) -> Just (MkDialog1 (handleDialog1FormMsg msg form))
  --             _ -> state.dialog
  --       }
  -- Dialog2FormMsg msg ->
  --   H.modify_ \state ->
  --     state
  --       { dialog =
  --           case state.dialog of
  --             Just (MkDialog2 form) -> Just (MkDialog2 (handleDialog2FormMsg msg form))
  --             _ -> state.dialog
  --       }
  render :: State -> H.ComponentHTML Action () m
  render state =
    let
      allCategories :: Array Category
      allCategories =
        state.book
          # Map.values
          # Array.fromFoldable
          # Array.concatMap (\tx -> [ tx.from.category, tx.to.category ])
          # nubBy (\a b -> compare a.name b.name)

      transactions :: Array (Tuple Int TransactionView)
      transactions =
        Array.sortBy
          (comparing (\(Tuple _ tx) -> tx.date))
          (Map.toUnfoldable state.book)

      transactionsWithBalance :: Array (Tuple Int TransactionViewWithBalance)
      transactionsWithBalance =
        let
          f ::
            (Tuple Decimal (Array (Tuple Int TransactionViewWithBalance))) ->
            (Tuple Int TransactionView) ->
            (Tuple Decimal (Array (Tuple Int TransactionViewWithBalance)))
          f (Tuple prevBalance txs) (Tuple transactionId o) =
            let
              newBalance =
                prevBalance
                  + ( if o.to.name == checkingAccount.name then
                        o.amount
                      else if o.from.name == checkingAccount.name then
                        -o.amount
                      else
                        Decimal.fromInt 0
                    )
            in
              ( Tuple newBalance
                  ( cons
                      ( Tuple
                          transactionId
                          { date: o.date
                          , descr: o.descr
                          , from: o.from
                          , to: o.to
                          , amount: o.amount
                          , balanceMovement: { from: prevBalance, to: newBalance }
                          }
                      )
                      txs
                  )
              )
        in
          Array.reverse
            $ snd
            $ Array.foldl
                f
                (Tuple (Decimal.fromInt 0) [])
                transactions
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
                                              [ HH.text $ amountFmt accountBalance
                                              ]
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
                , HH.div [ HP.class_ $ HH.ClassName "section" ]
                    [ HH.div [ HP.class_ $ HH.ClassName "transaction-list" ]
                        [ HH.div [ HP.class_ $ HH.ClassName "transaction-list__header" ]
                            [ HH.h3 [] [ HH.text "Transactions" ]
                            , HH.button
                                [ HP.class_ $ HH.ClassName "button button--primary"
                                , HE.onClick (const AddTransactionClicked)
                                ]
                                [ HH.text "Add Transaction" ]
                            ]
                        , HH.ul [ HP.class_ $ HH.ClassName "transaction-list__items" ]
                            ( map
                                ( \(Tuple transactionId tx) ->
                                    let
                                      isPositive = tx.amount > Decimal.fromInt 0 && tx.from /= checkingAccount

                                      amountClass =
                                        if isPositive then
                                          "transaction-item__amount transaction-item__amount--positive"
                                        else
                                          "transaction-item__amount transaction-item__amount--negative"

                                      amountSign =
                                        if isPositive then
                                          "+"
                                        else
                                          "-"
                                    in
                                      HH.li
                                        [ HP.class_ $ HH.ClassName "transaction-item"
                                        , HE.onClick (const $ EditTransactionClicked transactionId)
                                        ]
                                        [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__row" ]
                                            [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__main-content" ]
                                                [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__details" ]
                                                    [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__description" ]
                                                        [ HH.text tx.descr ]
                                                    , HH.div [ HP.class_ $ HH.ClassName "transaction-item__accounts" ]
                                                        [ HH.text (tx.from.name <> " → " <> tx.to.name) ]
                                                    ]
                                                , HH.div [ HP.class_ $ HH.ClassName "transaction-item__date" ]
                                                    [ HH.text (dateFmt tx.date) ]
                                                , HH.div [ HP.class_ $ HH.ClassName amountClass ]
                                                    [ HH.text $ amountSign <> amountFmt tx.amount ]
                                                ]
                                            , HH.div [ HP.class_ $ HH.ClassName "transaction-item__balance-column" ]
                                                [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__balance-movement" ]
                                                    [ HH.span [ HP.class_ $ HH.ClassName "balance-before" ] [ HH.text (amountFmt tx.balanceMovement.from) ]
                                                    , HH.span [ HP.class_ $ HH.ClassName "arrow-icon" ] [ HH.text " → " ]
                                                    , HH.span [ HP.class_ $ HH.ClassName "balance-after" ] [ HH.text (amountFmt tx.balanceMovement.to) ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                )
                                transactionsWithBalance
                            )
                        ]
                    ]
                , case state.dialog of
                    Nothing -> HH.text ""
                    Just (EditDialog data_) -> viewEditDialog data_
                    Just (CreateDialog data_) -> viewCreateDialog data_
                ]
            ]
        ]

  -- viewEditDialog :: EditDialog -> H.ComponentHTML Action () m
  viewEditDialog form =
    HH.dialog [ HP.id myDialog2Id ]
      [ HH.text "TODO edit" ]

  -- viewCreateDialog :: CreateDialog -> H.ComponentHTML Action () m
  viewCreateDialog form =
    HH.dialog [ HP.id myDialog1Id ]
      [ HH.text "TODO create" ]
