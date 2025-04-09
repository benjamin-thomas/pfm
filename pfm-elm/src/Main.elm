port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Decimal exposing (Decimal, zero)
import Dict exposing (Dict)
import Domain exposing (Account, Category, TransactionViewWithBalance)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Iso8601
import Json.Encode as E
import Page.UI as UI_Page
import Route exposing (Route)
import Set
import Task exposing (Task)
import Time
import Url exposing (Url)
import Utils exposing (formatDateForInput)


port escapePressed : (() -> msg) -> Sub msg


port enterPressed : (() -> msg) -> Sub msg


port consoleLogRaw : E.Value -> Cmd msg


consoleLog : String -> E.Value -> Cmd msg
consoleLog str value =
    consoleLogRaw <|
        E.object
            [ ( "title", E.string str )
            , ( "data", value )
            ]


port toggleTheme : () -> Cmd msg


type alias Category =
    { name : String
    }


type alias Account =
    { name : String
    , category : Category
    }


type alias Transaction =
    { date : Time.Posix
    , descr : String
    , fromAccountId : Int
    , toAccountId : Int
    , amount : Decimal
    }


type alias TransactionView =
    { date : Time.Posix
    , descr : String
    , from : Account
    , to : Account
    , amount : Decimal
    }


assets : Category
assets =
    { name = "Assets" }


expenses : Category
expenses =
    { name = "Expenses" }


equity : Category
equity =
    { name = "Equity" }


income : Category
income =
    { name = "Income" }


checkingAccount : Account
checkingAccount =
    { name = "Checking account"
    , category = assets
    }


savingsAccount : Account
savingsAccount =
    { name = "Savings account"
    , category = assets
    }


openingBalance : Account
openingBalance =
    { name = "OpeningBalance"
    , category = equity
    }


employerABC : Account
employerABC =
    { name = "EmployerABC"
    , category = income
    }


customerXYZ : Account
customerXYZ =
    { name = "CustomerXYZ"
    , category = income
    }


spar : Account
spar =
    { name = "Spar"
    , category = expenses
    }


tesco : Account
tesco =
    { name = "Tesco"
    , category = expenses
    }


amazon : Account
amazon =
    { name = "Amazon"
    , category = expenses
    }


allAccounts_ : List Account
allAccounts_ =
    [ checkingAccount
    , savingsAccount
    , openingBalance
    , employerABC
    , customerXYZ
    , spar
    , amazon
    ]


allAccounts : Dict String Account
allAccounts =
    allAccounts_
        |> List.map (\o -> ( o.name, o ))
        |> Dict.fromList


balance : Dict Int TransactionView -> Account -> Decimal
balance txs account =
    Dict.foldl
        (\_ { from, to, amount } total ->
            if to == account then
                Decimal.add total amount

            else if from == account then
                Decimal.sub total amount

            else
                total
        )
        zero
        txs


type alias MkEditDialog =
    { transactionId : Int
    , descr : String
    , from : String
    , to : String
    , amount : String
    , date : String
    , showTime : Bool
    }


type alias MkCreateDialog =
    { descr : String
    , from : String
    , to : String
    , amount : String
    , date : String
    , showTime : Bool
    }


type Dialog
    = EditDialog MkEditDialog
    | CreateDialog MkCreateDialog


type Page
    = NotFound
    | Home
    | UI


{-| These types exist to circumvent browser limitations (it seems?) related to the ":focused" CSS pseudo class.

When using a fixed or absolute position (for the dialog), then ":focused" pseudo class won't get "set" by the browser,
when initiating a "focus" action, programmatically.

Since I want to automatically "focus" an element, after dialog open, I need to track the state of the "just focused"
state (the pseudo class does kick-in after user action, such as modifying an input's value).

The type carries with it its CSS id.

-}
type JustFocusable
    = FocusableAmount String
    | FocusableDescr String


focusedInputToString : JustFocusable -> String
focusedInputToString justFocused =
    case justFocused of
        FocusableAmount str ->
            str

        FocusableDescr str ->
            str


amountField : JustFocusable
amountField =
    FocusableAmount "amount-field"


descriptionField : JustFocusable
descriptionField =
    FocusableDescr "description-field"


type alias Model =
    { key : Nav.Key
    , url : Url
    , route : Route
    , now : Time.Posix
    , zone : Time.Zone
    , justFocused : Maybe JustFocusable
    , book : Dict Int TransactionView
    , dialog : Maybe Dialog
    , isDarkTheme : Bool
    }


type MkEditDialogChanged
    = EditDescrChanged String
    | EditFromChanged String
    | EditToChanged String
    | EditAmountChanged String
    | EditDateChanged String
    | EditToggleTimeDisplay
    | EditDialogSave


type MkCreateDialogChanged
    = CreateDescrChanged String
    | CreateFromChanged String
    | CreateToChanged String
    | CreateAmountChanged String
    | CreateDateChanged String
    | CreateToggleTimeDisplay
    | CreateDialogSave


type Msg
    = FocusFailed
    | FocusOk
    | SetJustFocused (Maybe JustFocusable)
    | UrlRequested UrlRequest
    | UrlChanged Url
    | EditTransactionClicked Int
    | EditDialogChanged MkEditDialogChanged
    | CreateDialogChanged MkCreateDialogChanged
    | AddTransactionClicked
    | AfterFocus (Result Dom.Error ())
    | EscapedPressed
    | EnterPressed
    | GotTime Time.Posix
    | GotZone Time.Zone
    | ToggleTheme


amountFmt : Decimal -> String
amountFmt amount =
    let
        str =
            case Decimal.toString amount |> String.split "." of
                [ euros, cents ] ->
                    euros ++ "." ++ String.padRight 2 '0' cents

                [ euros ] ->
                    euros ++ ".00"

                _ ->
                    "IMPOSSIBLE"
    in
    str ++ "\u{00A0}€"


uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy toComparable =
    Tuple.first
        << List.foldl
            (\item ( items, keys ) ->
                if Set.member (toComparable item) keys then
                    ( items, keys )

                else
                    ( item :: items, Set.insert (toComparable item) keys )
            )
            ( [], Set.empty )


dateFmt : Time.Posix -> String
dateFmt =
    String.left 10
        << Iso8601.fromTime


view model =
    let
        viewPage =
            case model.route of
                Route.NotFound ->
                    H.div [] [ H.text "Not Found" ]

                Route.Home ->
                    viewHome model

                Route.UI ->
                    UI_Page.view
    in
    { title = "Personal Finance Manager"
    , body =
        [ H.div
            [ HA.classList
                [ ( "app", True )
                , ( "dark-theme", model.isDarkTheme )
                ]
            ]
            [ --cssLink
              themeToggleButton model.isDarkTheme
            , viewPage
            , case model.dialog of
                Nothing ->
                    H.text ""

                Just (EditDialog data) ->
                    dialog
                        [ HA.class "transaction"
                        , HA.attribute "open" ""
                        ]
                        [ viewEditDialog model.justFocused data ]

                Just (CreateDialog data) ->
                    dialog
                        [ HA.class "transaction"
                        , HA.attribute "open" ""
                        ]
                        [ viewCreateDialog model.justFocused data ]
            ]
        ]
    }


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    H.node "dialog"


viewHome : Model -> Html Msg
viewHome model =
    let
        allCategories : List Category
        allCategories =
            model.book
                |> Dict.values
                |> List.concatMap (\tx -> [ tx.from.category, tx.to.category ])
                |> uniqueBy .name

        transactions : List ( Int, TransactionView )
        transactions =
            List.sortBy
                (\( _, tx ) -> Time.posixToMillis tx.date)
                (Dict.toList model.book)

        transactions2 : List ( Int, TransactionViewWithBalance )
        transactions2 =
            let
                f :
                    ( Int, TransactionView )
                    -> ( Decimal, List ( Int, TransactionViewWithBalance ) )
                    -> ( Decimal, List ( Int, TransactionViewWithBalance ) )
                f ( transactionId, o ) ( prevBalance, txs ) =
                    let
                        newBalance =
                            Decimal.add
                                prevBalance
                                (if o.to.name == checkingAccount.name then
                                    o.amount

                                 else if o.from.name == checkingAccount.name then
                                    Decimal.negate o.amount

                                 else
                                    zero
                                )
                    in
                    ( newBalance
                    , ( transactionId
                      , { date = o.date
                        , descr = o.descr
                        , from = o.from
                        , to = o.to
                        , amount = o.amount
                        , balanceMovement = { from = prevBalance, to = newBalance }
                        }
                      )
                        :: txs
                    )
            in
            List.reverse <|
                Tuple.second <|
                    List.foldl
                        f
                        ( zero, [] )
                        transactions
    in
    H.div [ HA.class "container" ]
        [ H.div [ HA.class "section" ]
            [ H.div [ HA.class "debug-info" ]
                [ H.a [ Route.href Route.UI ] [ H.text "Go to UI" ] ]
            ]
        , H.div [ HA.class "section" ]
            [ H.h2 [ HA.class "section-title" ] [ H.text "Balances" ]
            , H.div [ HA.class "balances" ]
                (List.concatMap
                    (\category ->
                        List.map
                            (\account ->
                                let
                                    accountBalance =
                                        balance model.book account

                                    colorAccent =
                                        if category.name == "Assets" then
                                            "#3498db"

                                        else if category.name == "Expenses" then
                                            "#e74c3c"

                                        else
                                            "#9b59b6"
                                in
                                H.div [ HA.class "balance-card", HA.style "border-left-color" colorAccent ]
                                    [ H.div [ HA.class "balance-card__category" ] [ H.text category.name ]
                                    , H.div [ HA.class "balance-card__account" ] [ H.text account.name ]
                                    , H.div [ HA.class "balance-card__amount" ] [ H.text (amountFmt accountBalance) ]
                                    ]
                            )
                            (List.filter (\o -> o.category == category) allAccounts_)
                    )
                    (List.sortBy (\c -> c.name) <|
                        List.filter
                            (\c ->
                                List.member c
                                    [ assets

                                    -- , expenses
                                    -- , income
                                    ]
                            )
                            allCategories
                    )
                )
            ]
        , H.div [ HA.class "section" ]
            [ H.div [ HA.class "transaction-list" ]
                [ H.div [ HA.class "transaction-list__header" ]
                    [ H.h3 [] [ H.text "Transactions" ]
                    , H.button
                        [ HA.class "button button--primary"
                        , HE.onClick AddTransactionClicked
                        ]
                        [ H.text "Add Transaction" ]
                    ]
                , H.ul [ HA.class "transaction-list__items" ]
                    (List.map
                        (\( transactionId, tx ) ->
                            let
                                isPositive =
                                    Decimal.gt tx.amount Decimal.zero && tx.from /= checkingAccount

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
                            H.li
                                [ HA.class "transaction-item"
                                , HE.onClick (EditTransactionClicked transactionId)
                                ]
                                [ H.div [ HA.class "transaction-item__row" ]
                                    [ H.div [ HA.class "transaction-item__main-content" ]
                                        [ H.div [ HA.class "transaction-item__details" ]
                                            [ H.div [ HA.class "transaction-item__description" ]
                                                [ H.text tx.descr ]
                                            , H.div [ HA.class "transaction-item__accounts" ]
                                                [ H.text (tx.from.name ++ " → " ++ tx.to.name) ]
                                            ]
                                        , H.div [ HA.class "transaction-item__date" ]
                                            [ H.text (dateFmt tx.date) ]
                                        , H.div [ HA.class amountClass ]
                                            [ H.text (amountSign ++ amountFmt tx.amount) ]
                                        ]
                                    , H.div [ HA.class "transaction-item__balance-column" ]
                                        [ H.div [ HA.class "transaction-item__balance-movement" ]
                                            [ H.span [ HA.class "balance-before" ] [ H.text (amountFmt tx.balanceMovement.from) ]
                                            , H.span [ HA.class "arrow-icon" ] [ H.text " → " ]
                                            , H.span [ HA.class "balance-after" ] [ H.text (amountFmt tx.balanceMovement.to) ]
                                            ]
                                        ]
                                    ]
                                ]
                        )
                        transactions2
                    )
                ]
            ]
        , case model.dialog of
            Nothing ->
                H.text ""

            Just (EditDialog data) ->
                dialog
                    [ HA.class "transaction"
                    , HA.attribute "open" ""
                    ]
                    [ viewEditDialog model.justFocused data ]

            Just (CreateDialog data) ->
                dialog
                    [ HA.class "transaction"
                    , HA.attribute "open" ""
                    ]
                    [ viewCreateDialog model.justFocused data ]
        ]


viewEditDialog : Maybe JustFocusable -> MkEditDialog -> Html Msg
viewEditDialog justFocused data =
    H.div [ HA.class "dialog-content", HE.onClick <| SetJustFocused Nothing ]
        [ H.h3 [ HA.class "dialog-title" ] [ H.text "Edit Transaction" ]
        , makeField
            { text = "Description"
            , value = data.descr
            , onInput = \str -> EditDialogChanged (EditDescrChanged str)
            , justFocused = justFocused
            , field = descriptionField
            }
        , accountSelect
            { text = "From"
            , value = data.from
            , onInput = \str -> EditDialogChanged (EditFromChanged str)
            , accounts = allAccounts_
            , excludeAccount = Just data.to
            }
        , accountSelect
            { text = "To"
            , value = data.to
            , onInput = \str -> EditDialogChanged (EditToChanged str)
            , accounts = allAccounts_
            , excludeAccount = Just data.from
            }
        , makeField
            { text = "Amount"
            , value = data.amount
            , onInput = \str -> EditDialogChanged (EditAmountChanged str)
            , justFocused = justFocused
            , field = amountField
            }
        , dateField
            { text = "Date"
            , date = data.date
            , showTime = data.showTime
            , onDateInput = \str -> EditDialogChanged (EditDateChanged str)
            , onToggleTime = EditDialogChanged EditToggleTimeDisplay
            }
        , H.div [ HA.class "dialog-actions" ]
            [ H.button
                [ HA.class "button button--secondary"
                , HE.onClick EscapedPressed
                ]
                [ H.text "Cancel" ]
            , H.button
                [ HA.class "button button--primary"
                , HE.onClick (EditDialogChanged EditDialogSave)
                ]
                [ H.text "Save" ]
            ]
        ]


viewCreateDialog : Maybe JustFocusable -> MkCreateDialog -> Html Msg
viewCreateDialog justFocused data =
    H.div [ HA.class "dialog-content", HE.onClick <| SetJustFocused Nothing ]
        [ H.h3 [ HA.class "dialog-title" ] [ H.text "Add Transaction" ]
        , makeField
            { text = "Description"
            , value = data.descr
            , onInput = \str -> CreateDialogChanged (CreateDescrChanged str)
            , field = descriptionField
            , justFocused = justFocused
            }
        , accountSelect
            { text = "From"
            , value = data.from
            , onInput = \str -> CreateDialogChanged (CreateFromChanged str)
            , accounts = allAccounts_
            , excludeAccount = Just data.to
            }
        , accountSelect
            { text = "To"
            , value = data.to
            , onInput = \str -> CreateDialogChanged (CreateToChanged str)
            , accounts = allAccounts_
            , excludeAccount = Just data.from
            }
        , makeField
            { text = "Amount"
            , value = data.amount
            , onInput = \str -> CreateDialogChanged (CreateAmountChanged str)
            , field = amountField
            , justFocused = justFocused
            }
        , dateField
            { text = "Date"
            , date = data.date
            , showTime = data.showTime
            , onDateInput = \str -> CreateDialogChanged (CreateDateChanged str)
            , onToggleTime = CreateDialogChanged CreateToggleTimeDisplay
            }
        , H.div [ HA.class "dialog-actions" ]
            [ H.button
                [ HA.class "button button--secondary"
                , HE.onClick EscapedPressed
                ]
                [ H.text "Cancel" ]
            , H.button
                [ HA.class "button button--primary"
                , HE.onClick (CreateDialogChanged CreateDialogSave)
                ]
                [ H.text "Add" ]
            ]
        ]


dateField : { text : String, date : String, showTime : Bool, onDateInput : String -> msg, onToggleTime : msg } -> Html msg
dateField { text, date, showTime, onDateInput, onToggleTime } =
    let
        -- Parse the current date string
        dateOnly =
            if String.contains "T" date then
                String.split "T" date |> List.head |> Maybe.withDefault date

            else
                date

        timeOnly =
            if String.contains "T" date then
                String.split "T" date
                    |> List.drop 1
                    |> List.head
                    |> Maybe.withDefault "00:00"

            else
                "00:00"

        inputType =
            if showTime then
                "datetime-local"

            else
                "date"

        inputValue =
            if showTime then
                if String.contains "T" date then
                    date

                else
                    date ++ "T" ++ timeOnly

            else
                dateOnly
    in
    H.div [ HA.class "field" ]
        [ H.div [ HA.class "field__header" ]
            [ H.label [ HA.class "field__label" ]
                [ H.text text ]
            , H.div [ HA.class "field__toggle" ]
                [ H.label [ HA.class "toggle" ]
                    [ H.input
                        [ HA.type_ "checkbox"
                        , HA.checked showTime
                        , HE.onClick onToggleTime
                        , HA.class "toggle__input"
                        ]
                        []
                    , H.span [ HA.class "toggle__label" ] [ H.text "Include time" ]
                    ]
                ]
            ]
        , H.input
            [ HA.class "field__input"
            , HA.type_ inputType
            , HA.value inputValue
            , HE.onInput
                (\newValue ->
                    if showTime then
                        onDateInput newValue

                    else
                    -- Preserve any existing time when changing just the date
                    if
                        String.contains "T" date
                    then
                        onDateInput (newValue ++ "T" ++ timeOnly)

                    else
                        onDateInput newValue
                )
            ]
            []
        ]


makeField : { a | onInput : String -> Msg, text : String, value : String, field : JustFocusable, justFocused : Maybe JustFocusable } -> Html Msg
makeField { onInput, text, value, field, justFocused } =
    H.div [ HA.class "field" ]
        [ H.label [ HA.class "field__label" ]
            [ H.text text ]
        , H.input
            [ HA.classList
                [ ( "field__input", True )
                , ( "just-focused", justFocused == Just field )
                ]
            , HA.value value
            , HE.onInput onInput
            , HE.onClick (SetJustFocused <| Just field)
            , HA.id (focusedInputToString field)
            ]
            []
        ]


accountSelect : { a | onInput : String -> msg, text : String, value : String, accounts : List Account, excludeAccount : Maybe String } -> Html msg
accountSelect { onInput, text, value, accounts, excludeAccount } =
    let
        filteredAccounts =
            case excludeAccount of
                Just excludeName ->
                    if String.isEmpty excludeName then
                        accounts

                    else
                        List.filter (\account -> account.name /= excludeName) accounts

                Nothing ->
                    accounts
    in
    H.div [ HA.class "field" ]
        [ H.label [ HA.class "field__label" ]
            [ H.text text ]
        , H.select
            [ HA.class "field__select"
            , HE.onInput onInput
            , HA.value value
            ]
            (H.option [ HA.value "" ] [ H.text "-- Select an account --" ]
                :: List.map
                    (\account ->
                        H.option
                            [ HA.value account.name
                            , HA.selected (account.name == value)
                            ]
                            [ H.text (account.category.name ++ ": " ++ account.name) ]
                    )
                    filteredAccounts
            )
        ]


onDay : Int -> Time.Posix
onDay n =
    {-
       $ date -d '1 week ago 14:00'
       dim. 23 mars 2025 14:00:00 CET

       $ date -d '1 week ago 14:00' +%s
       1742734800
    -}
    let
        offset =
            1742734800 * 1000

        oneDay =
            60 * 60 * 24 * 1000
    in
    Time.millisToPosix <| n * oneDay + offset


type alias Flags =
    { language : String
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        v : String -> Decimal
        v =
            Maybe.withDefault zero << Decimal.fromString

        book : Dict Int TransactionView
        book =
            Dict.fromList
                [ ( 1
                  , { date = onDay 0
                    , descr = "Opening balance"
                    , from = openingBalance
                    , to = checkingAccount
                    , amount = v "1000.00"
                    }
                  )
                , ( 2
                  , { date = onDay 1
                    , descr = "Groceries"
                    , from = checkingAccount
                    , to = spar
                    , amount = v "9.99"
                    }
                  )
                , ( 3
                  , { date = onDay 2
                    , descr = "Book purchase"
                    , from = checkingAccount
                    , to = amazon
                    , amount = v "54.99"
                    }
                  )
                , ( 4
                  , { date = onDay 3
                    , descr = "Groceries, again"
                    , from = checkingAccount
                    , to = spar
                    , amount = v "37.42"
                    }
                  )
                , ( 5
                  , { date = onDay 4
                    , descr = "Salary"
                    , from = employerABC
                    , to = checkingAccount
                    , amount = v "100.00"
                    }
                  )
                ]
    in
    ( { key = key
      , url = url
      , route = Route.fromUrl url
      , now = Time.millisToPosix 0
      , zone = Time.utc
      , justFocused = Nothing
      , book = book
      , dialog = Nothing
      , isDarkTheme = False
      }
    , Task.perform GotZone Time.here
    )


focusField : JustFocusable -> Cmd Msg
focusField field =
    Task.attempt
        AfterFocus
        (Dom.focus <| focusedInputToString field)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AfterFocus result ->
            case result of
                Ok () ->
                    ( model
                    , Cmd.none
                    )

                Err (Dom.NotFound str) ->
                    ( model
                    , consoleLog
                        "Focusing failed"
                        (E.object
                            [ ( "fieldName", E.string str )
                            ]
                        )
                    )

        SetJustFocused x ->
            ( { model | justFocused = x }
            , Cmd.none
            )

        FocusOk ->
            ( model, Cmd.none )

        FocusFailed ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            ( { model
                | url = url
                , route = Route.fromUrl url
              }
            , Cmd.none
            )

        EditTransactionClicked transactionId ->
            handleEditDialog transactionId model

        EditDialogChanged subMsg ->
            case model.dialog of
                Just (EditDialog data) ->
                    case subMsg of
                        EditDescrChanged str ->
                            ( { model
                                | dialog = Just <| EditDialog { data | descr = str }
                              }
                            , Cmd.none
                            )

                        EditFromChanged str ->
                            ( { model
                                | dialog = Just <| EditDialog { data | from = str }
                                , justFocused = Nothing
                              }
                            , Cmd.none
                            )

                        EditToChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | to = str } }
                            , Cmd.none
                            )

                        EditAmountChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | amount = str } }
                            , Cmd.none
                            )

                        EditDateChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | date = str } }
                            , Cmd.none
                            )

                        EditToggleTimeDisplay ->
                            ( { model | dialog = Just <| EditDialog { data | showTime = not data.showTime } }
                            , Cmd.none
                            )

                        EditDialogSave ->
                            let
                                newBook : Dict Int TransactionView
                                newBook =
                                    Dict.update
                                        data.transactionId
                                        (\mbOld ->
                                            mbOld
                                                |> Maybe.andThen
                                                    (\old ->
                                                        let
                                                            fromAccount =
                                                                Dict.get data.from allAccounts
                                                                    |> Maybe.withDefault old.from

                                                            toAccount =
                                                                Dict.get data.to allAccounts
                                                                    |> Maybe.withDefault old.to

                                                            newDate =
                                                                Iso8601.toTime data.date
                                                                    |> Result.toMaybe
                                                                    |> Maybe.withDefault old.date
                                                        in
                                                        Just
                                                            { old
                                                                | descr = data.descr
                                                                , from = fromAccount
                                                                , to = toAccount
                                                                , amount =
                                                                    Maybe.withDefault
                                                                        old.amount
                                                                        (Decimal.fromString data.amount)
                                                                , date = newDate
                                                            }
                                                    )
                                        )
                                        model.book
                            in
                            ( { model
                                | book = newBook
                                , dialog = Nothing
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AddTransactionClicked ->
            let
                field =
                    descriptionField
            in
            ( { model
                | justFocused = Just field
                , dialog =
                    Just <|
                        CreateDialog
                            { descr = ""
                            , from = ""
                            , to = ""
                            , amount = ""
                            , date = ""
                            , showTime = False
                            }
              }
            , focusField field
            )

        CreateDialogChanged subMsg ->
            case model.dialog of
                Just (CreateDialog data) ->
                    case subMsg of
                        CreateDescrChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | descr = str } }
                            , Cmd.none
                            )

                        CreateFromChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | from = str } }
                            , Cmd.none
                            )

                        CreateToChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | to = str } }
                            , Cmd.none
                            )

                        CreateAmountChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | amount = str } }
                            , Cmd.none
                            )

                        CreateDateChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | date = str } }
                            , Cmd.none
                            )

                        CreateToggleTimeDisplay ->
                            ( { model | dialog = Just <| CreateDialog { data | showTime = not data.showTime } }
                            , Cmd.none
                            )

                        CreateDialogSave ->
                            let
                                newTransaction : TransactionView
                                newTransaction =
                                    let
                                        fromAccount =
                                            Dict.get data.from allAccounts
                                                |> Maybe.withDefault checkingAccount

                                        toAccount =
                                            Dict.get data.to allAccounts
                                                |> Maybe.withDefault spar

                                        parsedDate =
                                            Iso8601.toTime data.date
                                                |> Result.toMaybe
                                                |> Maybe.withDefault model.now
                                    in
                                    { date = parsedDate
                                    , descr = data.descr
                                    , from = fromAccount
                                    , to = toAccount
                                    , amount = Maybe.withDefault zero <| Decimal.fromString data.amount
                                    }
                            in
                            ( { model
                                | dialog = Nothing
                                , book = Dict.insert (Dict.size model.book + 1) newTransaction model.book
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        EscapedPressed ->
            ( { model | dialog = Nothing }
            , Cmd.none
            )

        EnterPressed ->
            case model.dialog of
                Just (EditDialog _) ->
                    update (EditDialogChanged EditDialogSave) model

                Just (CreateDialog _) ->
                    update (CreateDialogChanged CreateDialogSave) model

                _ ->
                    ( model
                    , Cmd.none
                    )

        GotTime now ->
            ( { model | now = now }
            , Cmd.none
            )

        GotZone zone ->
            ( { model | zone = zone }
            , Cmd.none
            )

        ToggleTheme ->
            ( { model | isDarkTheme = not model.isDarkTheme }
            , toggleTheme ()
            )


handleEditDialog : Int -> Model -> ( Model, Cmd Msg )
handleEditDialog id model =
    case Dict.get id model.book of
        Just tx ->
            let
                -- Check if the date has time information (not just 00:00:00)
                dateString =
                    Iso8601.fromTime tx.date

                hasTimeInfo =
                    String.contains "T" dateString
                        && not (String.endsWith "T00:00:00.000Z" dateString)
                        && not (String.endsWith "T00:00:00Z" dateString)

                field =
                    amountField
            in
            ( { model
                | justFocused = Just field
                , dialog =
                    Just <|
                        EditDialog
                            { transactionId = id
                            , descr = tx.descr
                            , from = tx.from.name
                            , to = tx.to.name
                            , amount = Decimal.toString tx.amount
                            , date = formatDateForInput tx.date hasTimeInfo
                            , showTime = hasTimeInfo
                            }
              }
            , focusField field
            )

        Nothing ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ escapePressed (\() -> EscapedPressed)
        , enterPressed (\() -> EnterPressed)
        , Time.every (Debug.log "time" 9991000) GotTime
        ]


themeToggleButton : Bool -> Html Msg
themeToggleButton isDarkTheme =
    H.button
        [ HA.class "theme-toggle"
        , HE.onClick ToggleTheme
        , HA.title
            (if isDarkTheme then
                "Switch to Light Mode"

             else
                "Switch to Dark Mode"
            )
        ]
        [ H.span []
            [ H.text
                (if isDarkTheme then
                    "☀️"

                 else
                    "🌙"
                )
            ]
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
