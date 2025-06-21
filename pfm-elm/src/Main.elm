port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Decimal exposing (Decimal, zero)
import Dict exposing (Dict)
import Domain exposing (AccountOld, TransactionViewWithBalance)
import Generated.Decoder exposing (decodeAccountRead, decodeCategory, decodeLedgerLineSummary)
import Generated.Encoder exposing (encodeTransactionWrite)
import Generated.Types exposing (AccountRead, Category, LedgerLineSummary, TransactionWrite)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Iso8601
import Json.Decode as D
import Json.Encode as E
import Page.UI as UI_Page
import Process
import Route exposing (Route)
import Set
import Task
import Time
import Url exposing (Url)
import Utils exposing (amountFmt, formatDateForInput)


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


port showDialog : () -> Cmd msg


port closeDialog : () -> Cmd msg


{-| http -v localhost:8080/categories
-}
fetchCategories : Cmd Msg
fetchCategories =
    Http.get
        { url = "http://localhost:8080/categories"
        , expect = Http.expectJson GotCategories (D.list decodeCategory)
        }



{- | http -v localhost:8080/transactions/ accountId==2

-}


fetchLedgerLineSummary : Cmd Msg
fetchLedgerLineSummary =
    Http.get
        { url = "http://localhost:8080/transactions?accountId=2"
        , expect = Http.expectJson GotLedgerLineSummary (D.list decodeLedgerLineSummary)
        }


fetchAccounts : Cmd Msg
fetchAccounts =
    Http.get
        { url = "http://localhost:8080/accounts"
        , expect = Http.expectJson GotAccounts (D.list decodeAccountRead)
        }


postTransaction : TransactionWrite -> Cmd Msg
postTransaction transaction =
    Http.post
        { url = "http://localhost:8080/transactions"
        , body = Http.jsonBody (encodeTransactionWrite transaction)

        -- , expect = Http.expectJson GotLedgerLineSummary (D.list decodeLedgerLineSummary)
        , expect =
            Http.expectJson
                (CreateDialogChanged << GotCreateSaveResponse)
                (D.list decodeLedgerLineSummary)
        }


type alias CategoryOld =
    { name : String
    }


type alias AccountOld =
    { name : String
    , category : CategoryOld
    }


type alias TransactionOld =
    { date : Time.Posix
    , descr : String
    , from : AccountOld
    , to : AccountOld
    , amount : Decimal
    }


assets : CategoryOld
assets =
    { name = "Assets" }


expenses : CategoryOld
expenses =
    { name = "Expenses" }


equity : CategoryOld
equity =
    { name = "Equity" }


income : CategoryOld
income =
    { name = "Income" }


checkingAccount : AccountOld
checkingAccount =
    { name = "Checking account"
    , category = assets
    }


savingsAccount : AccountOld
savingsAccount =
    { name = "Savings account"
    , category = assets
    }


openingBalance : AccountOld
openingBalance =
    { name = "OpeningBalance"
    , category = equity
    }


employerABC : AccountOld
employerABC =
    { name = "EmployerABC"
    , category = income
    }


customerXYZ : AccountOld
customerXYZ =
    { name = "CustomerXYZ"
    , category = income
    }


spar : AccountOld
spar =
    { name = "Spar"
    , category = expenses
    }


tesco : AccountOld
tesco =
    { name = "Tesco"
    , category = expenses
    }


amazon : AccountOld
amazon =
    { name = "Amazon"
    , category = expenses
    }


allAccounts_ : List AccountOld
allAccounts_ =
    [ checkingAccount
    , savingsAccount
    , openingBalance
    , employerABC
    , customerXYZ
    , spar
    , amazon
    ]


allAccounts : Dict String AccountOld
allAccounts =
    allAccounts_
        |> List.map (\o -> ( o.name, o ))
        |> Dict.fromList


balance : Dict Int TransactionOld -> AccountOld -> Decimal
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
    , fromAccountId : Int
    , toAccountId : Int
    , amount : String
    , date : Time.Posix
    , showTime : Bool
    }


type alias MkCreateDialog =
    { descr : String
    , fromAccountId : Int
    , toAccountId : Int
    , amount : String
    , date : Time.Posix
    , showTime : Bool
    }


type Dialog
    = EditDialog MkEditDialog
    | CreateDialog MkCreateDialog


type Page
    = NotFound
    | Home
    | UI


type Status a
    = Loading
    | Failed
    | Loaded a


type alias Model =
    { key : Nav.Key
    , url : Url
    , route : Route
    , now : Time.Posix
    , zone : Time.Zone
    , book : Dict Int TransactionOld
    , book2 : Status (List LedgerLineSummary)
    , dialog : Maybe Dialog
    , isDarkTheme : Bool
    , tempCategories : Status (List Category)
    , accounts : Status (List AccountRead)
    }


type MkEditDialogChanged
    = EditDescrChanged String
    | EditFromChanged Int
    | EditToChanged Int
    | EditAmountChanged String
    | EditDateChanged Time.Posix
    | EditToggleTimeDisplay
    | EditDialogSave
    | GotEditSaveResponse (Result Http.Error (List LedgerLineSummary))


type MkCreateDialogChanged
    = CreateDescrChanged String
    | CreateFromChanged Int
    | CreateToChanged Int
    | CreateAmountChanged String
    | CreateDateChanged Time.Posix
    | CreateToggleTimeDisplay
    | CreateDialogSave
    | GotCreateSaveResponse (Result Http.Error (List LedgerLineSummary))


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | EditTransactionClicked Int
    | EditDialogChanged MkEditDialogChanged
    | CreateDialogChanged MkCreateDialogChanged
    | AddTransactionClicked
    | OpenCreateDialog Time.Posix
    | CloseDialogPressed
    | EnterPressed
    | GotZone Time.Zone
    | ToggleTheme
    | GotCategories (Result Http.Error (List Category))
    | GotAccounts (Result Http.Error (List AccountRead))
    | GotLedgerLineSummary (Result Http.Error (List LedgerLineSummary))


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


view : Model -> Browser.Document Msg
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
            [ themeToggleButton model.isDarkTheme
            , viewPage
            ]
        ]
    }


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


dialog : List (Attribute msg) -> List (Html msg) -> Html msg
dialog =
    H.node "dialog"


accountsForBalances : List AccountOld
accountsForBalances =
    List.sortBy (\o -> o.name) <|
        List.filter
            (\o -> o.category == assets)
            allAccounts_


balanceCard : AccountOld -> Decimal -> Html Msg
balanceCard account accountBalance =
    let
        colorAccent =
            if account.category.name == "Assets" then
                "#3498db"

            else if account.category.name == "Expenses" then
                "#e74c3c"

            else
                "#9b59b6"
    in
    H.div
        [ HA.class "balance-card", HA.style "border-left-color" colorAccent ]
        [ H.div [ HA.class "balance-card__category" ] [ H.text account.category.name ]
        , H.div [ HA.class "balance-card__account" ] [ H.text account.name ]
        , H.div [ HA.class "balance-card__amount" ] [ H.text (amountFmt accountBalance) ]
        ]


withPriorBalance : List LedgerLineSummary -> List ( LedgerLineSummary, ( Int, String ) )
withPriorBalance transactions =
    case transactions of
        [] ->
            []

        first :: rest ->
            ( first, ( 0, "0.00" ) )
                :: List.map2
                    (\tx prevTx ->
                        ( tx
                        , ( prevTx.runningBalanceCents
                          , prevTx.runningBalance
                          )
                        )
                    )
                    rest
                    transactions


viewOneTransaction : ( LedgerLineSummary, ( Int, String ) ) -> Html Msg
viewOneTransaction ( tx, ( priorBalanceCents, priorBalance ) ) =
    let
        isPositive =
            --Decimal.gt tx.amount Decimal.zero && tx.from /= checkingAccount
            tx.flowCents > 0

        amountClass =
            if isPositive then
                "transaction-item__amount transaction-item__amount--positive"

            else
                "transaction-item__amount transaction-item__amount--negative"

        amountSign =
            if isPositive then
                "+"

            else
                ""
    in
    H.li
        [ HA.class "transaction-item"
        , HE.onClick (EditTransactionClicked tx.transactionId)
        ]
        [ H.div [ HA.class "transaction-item__row" ]
            [ H.div [ HA.class "transaction-item__main-content" ]
                [ H.div [ HA.class "transaction-item__details" ]
                    [ H.div [ HA.class "transaction-item__description" ]
                        [ H.text tx.descr ]
                    , H.div [ HA.class "transaction-item__accounts" ]
                        [ H.text (tx.fromAccountName ++ " → " ++ tx.toAccountName) ]
                    ]
                , H.div [ HA.class "transaction-item__date" ]
                    [ H.text (dateFmt <| Time.millisToPosix <| tx.dateUnix * 1000) ]
                , H.div [ HA.class amountClass ]
                    [ H.text (amountSign ++ tx.flow ++ "\u{00A0}€") ]
                ]
            , H.div [ HA.class "transaction-item__balance-column" ]
                [ H.div [ HA.class "transaction-item__balance-movement" ]
                    [ H.span [ HA.class "balance-before" ] [ H.text <| priorBalance ++ "\u{00A0}€" ]
                    , H.span [ HA.class "arrow-icon" ] [ H.text " → " ]
                    , H.span [ HA.class "balance-after" ] [ H.text <| tx.runningBalance ++ "\u{00A0}€" ]
                    ]
                ]
            ]
        ]


viewLedgerLineSummary : List LedgerLineSummary -> Html Msg
viewLedgerLineSummary withRunningBalanceEntity =
    H.div [ HA.class "section" ]
        [ H.div [ HA.class "transaction-list" ]
            [ H.div [ HA.class "transaction-list__header" ]
                [ H.h3 [] [ H.text "Transactions2 (from DB)" ]
                , H.button
                    [ HA.class "button button--primary"
                    , HE.onClick AddTransactionClicked
                    ]
                    [ H.text "Add Transaction" ]
                ]
            , H.ul [ HA.class "transaction-list__items" ]
                (List.map
                    viewOneTransaction
                    (withPriorBalance withRunningBalanceEntity)
                )
            ]
        ]


viewHome : Model -> Html Msg
viewHome model =
    H.div [ HA.class "container" ]
        [ H.div [ HA.class "section" ]
            [ H.div [ HA.class "debug-info" ]
                [ H.a [ Route.href Route.UI ] [ H.text "Go to UI" ] ]
            ]
        , H.h1 [ HA.style "margin-bottom" "0" ] [ H.text "PFM" ]
        , H.h4 [ HA.style "margin-top" "3px", HA.style "margin-bottom" "8px" ] [ H.text "In Elm" ]
        , H.div [ HA.class "section" ]
            [ H.h2 [ HA.class "section-title" ] [ H.text "Balances" ]
            , H.div [ HA.class "balances" ]
                (List.map
                    (\account ->
                        balanceCard account (balance model.book account)
                    )
                    accountsForBalances
                )
            ]
        , H.div []
            [ H.h2 [] [ H.text "TEMP: testing frontend/backend code-gen interaction" ]
            , case model.tempCategories of
                Loading ->
                    H.div []
                        [ H.text "Loading categories..." ]

                Failed ->
                    H.div []
                        [ H.text "Error loading categories." ]

                Loaded categories ->
                    H.div []
                        [ H.text "Categories loaded."
                        , H.ul []
                            (List.map
                                (\category ->
                                    H.li []
                                        [ H.text category.name ]
                                )
                                categories
                            )
                        ]
            ]
        , case model.book2 of
            Loading ->
                H.text "Loading transactions..."

            Failed ->
                H.text "Error loading transactions."

            Loaded withRunningBalanceEntity ->
                viewLedgerLineSummary withRunningBalanceEntity
        , case model.dialog of
            Nothing ->
                H.text ""

            Just (EditDialog data) ->
                case model.accounts of
                    Loaded allAccounts2 ->
                        viewEditDialog allAccounts2 data

                    _ ->
                        -- FIXME: that doesn't feel right
                        H.text "??"

            Just (CreateDialog data) ->
                case model.accounts of
                    Loaded allAccounts2 ->
                        viewCreateDialog allAccounts2 data

                    _ ->
                        -- FIXME: that doesn't feel right
                        H.text "??"
        ]


viewEditDialog : List AccountRead -> MkEditDialog -> Html Msg
viewEditDialog allAccounts2 data =
    dialog
        [ HA.id "transaction-dialog"
        , HA.class "transaction"
        ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [ HA.class "dialog-title" ] [ H.text "Edit Transaction" ]
            , makeTextField
                { text = "Description"
                , value = data.descr
                , onInput = EditDialogChanged << EditDescrChanged
                , autofocus = False
                }
            , accountSelect
                { text = "From"
                , value = String.fromInt data.fromAccountId
                , onInput = EditDialogChanged << EditFromChanged
                , accounts = allAccounts2
                , excludeAccount = Just (String.fromInt data.toAccountId)
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = EditDialogChanged << EditToChanged
                , accounts = allAccounts2
                , excludeAccount = Just (String.fromInt data.fromAccountId)
                }
            , makeTextField
                { text = "Amount"
                , value = data.amount
                , onInput = EditDialogChanged << EditAmountChanged
                , autofocus = True
                }
            , dateField
                { text = "Date"
                , date = data.date
                , showTime = data.showTime
                , onDateInput = EditDialogChanged << EditDateChanged
                , onToggleTime = EditDialogChanged EditToggleTimeDisplay
                }
            , H.div [ HA.class "dialog-actions" ]
                [ H.button
                    [ HA.class "button button--secondary"
                    , HE.onClick CloseDialogPressed
                    ]
                    [ H.text "Cancel" ]
                , H.button
                    [ HA.class "button button--primary"
                    , HE.onClick (EditDialogChanged EditDialogSave)
                    ]
                    [ H.text "Save" ]
                ]
            ]
        ]


viewCreateDialog : List AccountRead -> MkCreateDialog -> Html Msg
viewCreateDialog allAccounts2 data =
    dialog
        [ HA.id "transaction-dialog"
        , HA.class "transaction"
        ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [ HA.class "dialog-title" ] [ H.text "Add Transaction" ]
            , makeTextField
                { text = "Description"
                , value = data.descr
                , onInput = CreateDialogChanged << CreateDescrChanged
                , autofocus = False
                }
            , accountSelect
                { text = "From"
                , value = String.fromInt data.fromAccountId
                , onInput = CreateDialogChanged << CreateFromChanged
                , accounts = allAccounts2
                , excludeAccount = Just (String.fromInt data.toAccountId)
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = CreateDialogChanged << CreateToChanged
                , accounts = allAccounts2
                , excludeAccount = Just (String.fromInt data.fromAccountId)
                }
            , makeTextField
                { text = "Amount"
                , value = data.amount
                , onInput = CreateDialogChanged << CreateAmountChanged
                , autofocus = False
                }
            , dateField
                { text = "Date"
                , date = data.date
                , showTime = data.showTime
                , onDateInput = CreateDialogChanged << CreateDateChanged
                , onToggleTime = CreateDialogChanged CreateToggleTimeDisplay
                }
            , H.div [ HA.class "dialog-actions" ]
                [ H.button
                    [ HA.class "button button--secondary"
                    , HE.onClick CloseDialogPressed
                    ]
                    [ H.text "Cancel" ]
                , H.button
                    [ HA.class "button button--primary"
                    , HE.onClick (CreateDialogChanged CreateDialogSave)
                    ]
                    [ H.text "Add" ]
                ]
            ]
        ]


makeTextField : { text : String, value : String, onInput : String -> msg, autofocus : Bool } -> Html msg
makeTextField { text, value, onInput, autofocus } =
    let
        fieldId =
            makeFieldId text
    in
    H.div [ HA.class "field" ]
        [ H.label [ HA.class "field__label", HA.for fieldId ]
            [ H.text text ]
        , H.input
            (List.filterMap identity
                [ Just <| HA.type_ "text"
                , Just <| HA.id fieldId
                , Just <| HA.class "field__input"
                , Just <| HA.value value
                , Just <| HE.onInput onInput

                -- Don't use Elm's autofocus, we need to trigger HTML-native functionality for the dialog handling.
                , if autofocus then
                    Just <| HA.attribute "autofocus" ""

                  else
                    Nothing
                ]
            )
            []
        ]


makeFieldId : String -> String
makeFieldId =
    (\s -> s ++ "-field")
        << String.replace " " "-"
        << String.toLower


dateField :
    { text : String
    , date : Time.Posix
    , showTime : Bool
    , onDateInput : Time.Posix -> msg
    , onToggleTime : msg
    }
    -> Html msg
dateField { text, date, showTime, onDateInput, onToggleTime } =
    let
        fieldId =
            makeFieldId text

        inputType =
            -- If the time is not shown, we'll still store an "instant" (so a posix time)
            -- So the time portion will implicitly be equal to the current time, from the user's browser
            -- at the time of the input.
            if showTime then
                "datetime-local"

            else
                "date"
    in
    H.div [ HA.class "field" ]
        [ H.div [ HA.class "field__header" ]
            [ H.label
                [ HA.class "field__label"
                , HA.for fieldId
                ]
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
            , HA.id fieldId
            , HA.type_ inputType
            , HA.value <| formatDateForInput date showTime
            , HE.onInput
                (onDateInput
                    << Debug.log "wat"
                    << Result.withDefault (Time.millisToPosix 0)
                    << Iso8601.toTime
                )
            ]
            []
        ]


accountSelect : { a | onInput : Int -> msg, text : String, value : String, accounts : List AccountRead, excludeAccount : Maybe String } -> Html msg
accountSelect { onInput, text, value, accounts, excludeAccount } =
    let
        fieldId =
            makeFieldId text

        filteredAccounts : List AccountRead
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
        [ H.label
            [ HA.class "field__label"
            , HA.for fieldId
            ]
            [ H.text text ]
        , H.select
            [ HA.class "field__select"
            , HA.id fieldId
            , HE.onInput (onInput << Maybe.withDefault 0 << String.toInt)
            , HA.value value
            ]
            (H.option [ HA.value "0" ] [ H.text "-- Select an account --" ]
                :: List.map
                    (\account ->
                        H.option
                            [ HA.value (String.fromInt account.accountId)
                            , HA.selected (account.name == value)
                            ]
                            [ H.text (account.categoryName ++ ": " ++ account.name) ]
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

        book : Dict Int TransactionOld
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
      , book = book
      , book2 = Loading
      , dialog = Nothing
      , isDarkTheme = False
      , tempCategories = Loading
      , accounts = Loading
      }
    , Cmd.batch
        [ Task.perform GotZone Time.here
        , consoleLog "Booting up..." E.null
        , fetchCategories
        , fetchLedgerLineSummary
        , fetchAccounts
        ]
    )


simulateResponse : Cmd Msg
simulateResponse =
    Process.sleep 0
        |> Task.andThen (\() -> Task.succeed [])
        -- |> Task.andThen (\() -> Task.fail <| Http.BadStatus 500)
        |> Task.attempt (CreateDialogChanged << GotCreateSaveResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotLedgerLineSummary result ->
            case result of
                Ok withRunningBalanceEntity ->
                    ( { model | book2 = Loaded withRunningBalanceEntity }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | book2 = Failed }
                    , Cmd.none
                    )

        GotAccounts result ->
            case result of
                Ok accounts ->
                    ( { model | accounts = Loaded accounts }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | accounts = Failed }
                    , Cmd.none
                    )

        GotCategories result ->
            case result of
                Ok categories ->
                    ( { model | tempCategories = Loaded categories }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | tempCategories = Failed }
                    , Cmd.none
                    )

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

                        EditFromChanged n ->
                            ( { model
                                | dialog = Just <| EditDialog { data | fromAccountId = n }
                              }
                            , Cmd.none
                            )

                        EditToChanged n ->
                            ( { model
                                | dialog =
                                    Just <|
                                        EditDialog { data | toAccountId = n }
                              }
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
                                newBook : Dict Int TransactionOld
                                newBook =
                                    Dict.update
                                        data.transactionId
                                        (\mbOld ->
                                            mbOld
                                                |> Maybe.andThen
                                                    (\old ->
                                                        let
                                                            fromAccount =
                                                                -- Dict.get data.from allAccounts
                                                                --     |> Maybe.withDefault old.from
                                                                Debug.todo "fromAccount"

                                                            toAccount =
                                                                -- Dict.get data.to allAccounts
                                                                --     |> Maybe.withDefault old.to
                                                                Debug.todo "toAccount"
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
                                                                , date = data.date
                                                            }
                                                    )
                                        )
                                        model.book
                            in
                            ( { model
                                | book = newBook

                                -- , dialog = Nothing
                              }
                            , Cmd.batch
                                [ --closeDialog ()
                                  Task.attempt
                                    (EditDialogChanged << GotEditSaveResponse)
                                    (Task.succeed [])
                                ]
                            )

                        GotEditSaveResponse result ->
                            Debug.todo "GotSaveResponse"

                _ ->
                    ( model
                    , Cmd.none
                    )

        AddTransactionClicked ->
            ( model
            , Task.perform OpenCreateDialog Time.now
            )

        OpenCreateDialog now ->
            let
                dialog_ =
                    CreateDialog
                        { descr = ""
                        , fromAccountId = 0
                        , toAccountId = 0
                        , amount = ""
                        , date = now
                        , showTime = True
                        }
            in
            ( { model | dialog = Just dialog_ }
            , showDialog ()
            )

        CreateDialogChanged subMsg ->
            case model.dialog of
                Just (CreateDialog data) ->
                    case subMsg of
                        CreateDescrChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | descr = str } }
                            , Cmd.none
                            )

                        CreateFromChanged n ->
                            ( { model
                                | dialog =
                                    Just <|
                                        CreateDialog { data | fromAccountId = n }
                              }
                            , Cmd.none
                            )

                        CreateToChanged n ->
                            ( { model | dialog = Just <| CreateDialog { data | toAccountId = n } }
                            , Cmd.none
                            )

                        CreateAmountChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | amount = str } }
                            , Cmd.none
                            )

                        CreateDateChanged posix ->
                            ( { model | dialog = Just <| CreateDialog { data | date = posix } }
                            , Cmd.none
                            )

                        CreateToggleTimeDisplay ->
                            ( { model | dialog = Just <| CreateDialog { data | showTime = not data.showTime } }
                            , Cmd.none
                            )

                        CreateDialogSave ->
                            let
                                newTransaction : TransactionWrite
                                newTransaction =
                                    Debug.log "newTransaction" <|
                                        { fromAccountId = data.fromAccountId
                                        , toAccountId = data.toAccountId

                                        -- We remove sub-second values, as only JS does that and we don't need it!
                                        , date = Time.posixToMillis data.date // 1000
                                        , descr = data.descr
                                        , cents =
                                            data.amount
                                                |> String.replace "." ""
                                                |> Debug.log "TMP"
                                                |> String.toInt
                                                |> Maybe.withDefault 0
                                        }
                            in
                            ( model
                              -- , simulateResponse
                            , postTransaction newTransaction
                            )

                        GotCreateSaveResponse result ->
                            ( { model
                                | dialog = Nothing
                                , book2 =
                                    case result of
                                        Err _ ->
                                            Failed

                                        Ok lines ->
                                            Loaded lines
                              }
                            , closeDialog ()
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        CloseDialogPressed ->
            ( { model | dialog = Nothing }
            , closeDialog ()
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
                dateString =
                    Iso8601.fromTime tx.date

                hasTimeInfo =
                    String.contains "T" dateString
                        && not (String.endsWith "T00:00:00.000Z" dateString)
                        && not (String.endsWith "T00:00:00Z" dateString)
            in
            ( { model
                | dialog =
                    Just <|
                        EditDialog
                            -- { transactionId = id
                            -- , descr = tx.descr
                            -- , fromAccountId = tx.from
                            -- , to = tx.to.name
                            -- , amount = Decimal.toString tx.amount
                            -- , date = formatDateForInput tx.date hasTimeInfo
                            -- , showTime = hasTimeInfo
                            -- }
                            (Debug.todo "EditDialog")
              }
            , showDialog ()
            )

        Nothing ->
            ( model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ enterPressed (\() -> EnterPressed)
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
