port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Generated.Decoder
    exposing
        ( decodeAccountBalanceRead
        , decodeAccountRead
        , decodeCategory
        , decodeLedgerLineSummary
        )
import Generated.Encoder exposing (encodeTransactionWrite)
import Generated.Types
    exposing
        ( AccountBalanceRead
        , AccountRead
        , Category
        , LedgerLineSummary
        , TransactionWrite
        )
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
import Task
import Time
import Url exposing (Url)
import Utils


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


{-| http -v localhost:8080/transactions/ accountId==2
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


fetchBalances : Cmd Msg
fetchBalances =
    Http.get
        { url = "http://localhost:8080/accounts/balances?accountIds=2,3" -- FIXME: find a way this param
        , expect = Http.expectJson GotBalances (D.list decodeAccountBalanceRead)
        }


postTransaction : TransactionWrite -> Cmd Msg
postTransaction transaction =
    Http.post
        { url = "http://localhost:8080/transactions"
        , body = Http.jsonBody (encodeTransactionWrite transaction)
        , expect =
            Http.expectWhatever
                (CreateDialogChanged << GotCreateSaveResponse)
        }


putTransaction : ( Int, TransactionWrite ) -> Cmd Msg
putTransaction ( transactionId, transaction ) =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8080/transactions/" ++ String.fromInt transactionId
        , body = Http.jsonBody (encodeTransactionWrite transaction)
        , expect =
            Http.expectWhatever
                (EditDialogChanged << GotEditSaveResponse)
        , timeout = Nothing
        , tracker = Nothing
        }


type alias MkEditDialog =
    { transactionId : Int
    , fromAccountId : Int
    , toAccountId : Int
    , amount : String
    , date : Time.Posix
    , descr : String
    , showTime : Bool
    }


type alias MkCreateDialog =
    { fromAccountId : Int
    , toAccountId : Int
    , date : Time.Posix
    , descr : String
    , amount : String
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
    , ledger : Status (List LedgerLineSummary)
    , dialog : Maybe Dialog
    , isDarkTheme : Bool
    , categories : Status (List Category)
    , accounts : Status (List AccountRead)
    , balances : Status (List AccountBalanceRead)
    }


type MkEditDialogChanged
    = EditDescrChanged String
    | EditFromChanged Int
    | EditToChanged Int
    | EditAmountChanged String
    | EditDateChanged Time.Posix
    | EditToggleTimeDisplay
    | EditDialogSave
    | GotEditSaveResponse (Result Http.Error ())


type MkCreateDialogChanged
    = CreateDescrChanged String
    | CreateFromChanged Int
    | CreateToChanged Int
    | CreateAmountChanged String
    | CreateDateChanged Time.Posix
    | CreateToggleTimeDisplay
    | CreateDialogSave
    | GotCreateSaveResponse (Result Http.Error ())


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | EditTransactionClicked ( Int, TransactionWrite )
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
    | GotBalances (Result Http.Error (List AccountBalanceRead))
    | GotLedgerLineSummary (Result Http.Error (List LedgerLineSummary))


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


balanceCard : AccountBalanceRead -> Html Msg
balanceCard { categoryName, accountName, accountBalance } =
    let
        colorAccent =
            if categoryName == "Assets" then
                "#3498db"

            else if categoryName == "Expenses" then
                "#e74c3c"

            else
                "#9b59b6"
    in
    H.div
        [ HA.class "balance-card", HA.style "border-left-color" colorAccent ]
        [ H.div [ HA.class "balance-card__category" ] [ H.text categoryName ]
        , H.div [ HA.class "balance-card__account" ] [ H.text accountName ]
        , H.div [ HA.class "balance-card__amount" ]
            [ H.text <|
                Utils.amountFmt2 <|
                    { intPart = accountBalance // 100
                    , decPart = modBy 100 accountBalance
                    }
            ]
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


toTransactionWrite : LedgerLineSummary -> ( Int, TransactionWrite )
toTransactionWrite { transactionId, fromAccountId, toAccountId, dateUnix, descr, flowCents } =
    ( transactionId
    , { fromAccountId = fromAccountId
      , toAccountId = toAccountId
      , dateUnix = dateUnix
      , descr = descr
      , cents = abs flowCents
      }
    )


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
        , HE.onClick (EditTransactionClicked <| toTransactionWrite tx)
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
                    [ H.text (Utils.dateFmtUnix tx.dateUnix) ]
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
                [ H.h3 [] [ H.text "Transactions" ]
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
    case ( model.accounts, model.balances, model.ledger ) of
        ( Loaded accounts, Loaded balances, Loaded ledger ) ->
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
                            (\balance_ ->
                                balanceCard balance_
                            )
                            balances
                        )
                    ]
                , viewLedgerLineSummary ledger
                , case model.dialog of
                    Nothing ->
                        H.text ""

                    Just (EditDialog data) ->
                        viewEditDialog accounts data

                    Just (CreateDialog data) ->
                        viewCreateDialog accounts data
                ]

        ( Loaded _, _, _ ) ->
            H.text "Loading accounts..."

        ( _, Loaded _, _ ) ->
            H.text "Loading balances..."

        ( _, _, Loaded _ ) ->
            H.text "Loading ledger..."

        _ ->
            H.text "Error loading data."


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
                , excludeAccountId = Just data.toAccountId
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = EditDialogChanged << EditToChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.fromAccountId
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
                , excludeAccountId = Just data.toAccountId
                }
            , accountSelect
                { text = "To"
                , value = String.fromInt data.toAccountId
                , onInput = CreateDialogChanged << CreateToChanged
                , accounts = allAccounts2
                , excludeAccountId = Just data.fromAccountId
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
            , HA.value <| Utils.formatDateForInput date showTime
            , HE.onInput
                (onDateInput
                    << Debug.log "wat"
                    << Result.withDefault (Time.millisToPosix 0)
                    << Iso8601.toTime
                )
            ]
            []
        ]


accountSelect :
    { a
        | onInput : Int -> msg
        , text : String
        , value : String
        , accounts : List AccountRead
        , excludeAccountId : Maybe Int
    }
    -> Html msg
accountSelect { onInput, text, value, accounts, excludeAccountId } =
    let
        fieldId =
            makeFieldId text

        filteredAccounts : List AccountRead
        filteredAccounts =
            case excludeAccountId of
                Just excludeId ->
                    List.filter ((/=) excludeId << .accountId) accounts

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
                            , HA.selected (String.fromInt account.accountId == value)
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
    ( { key = key
      , url = url
      , route = Route.fromUrl url
      , now = Time.millisToPosix 0
      , zone = Time.utc
      , ledger = Loading
      , dialog = Nothing
      , isDarkTheme = False
      , categories = Loading
      , accounts = Loading
      , balances = Loading
      }
    , Cmd.batch
        [ Task.perform GotZone Time.here
        , consoleLog "Booting up..." E.null
        , fetchCategories
        , fetchLedgerLineSummary
        , fetchAccounts
        , fetchBalances
        ]
    )


simulateResponse : Cmd Msg
simulateResponse =
    Process.sleep 0
        |> Task.andThen (\() -> Task.succeed ())
        -- |> Task.andThen (\() -> Task.fail <| Http.BadStatus 500)
        |> Task.attempt (CreateDialogChanged << GotCreateSaveResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBalances result ->
            case result of
                Ok balances ->
                    ( { model | balances = Loaded balances }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | balances = Failed }
                    , Cmd.none
                    )

        GotLedgerLineSummary result ->
            case result of
                Ok withRunningBalanceEntity ->
                    ( { model | ledger = Loaded withRunningBalanceEntity }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | ledger = Failed }
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
                    ( { model | categories = Loaded categories }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | categories = Failed }
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

        EditTransactionClicked params ->
            handleEditDialog params model

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
                            ( model
                            , Cmd.batch
                                [ --closeDialog ()
                                  --   Task.attempt
                                  --     (EditDialogChanged << GotEditSaveResponse)
                                  --     (Task.succeed ())
                                  let
                                    transactionWrite : TransactionWrite
                                    transactionWrite =
                                        { fromAccountId = data.fromAccountId
                                        , toAccountId = data.toAccountId
                                        , dateUnix = Time.posixToMillis data.date // 1000
                                        , descr = data.descr
                                        , cents =
                                            String.toInt data.amount
                                                -- (-1) will make the update fail
                                                |> Maybe.withDefault -1
                                        }
                                  in
                                  putTransaction
                                    ( data.transactionId
                                    , transactionWrite
                                    )
                                ]
                            )

                        GotEditSaveResponse result ->
                            case result of
                                Err _ ->
                                    ( model
                                      -- TODO: display toast error or similar
                                    , Cmd.none
                                    )

                                Ok _ ->
                                    ( { model
                                        | dialog = Nothing
                                      }
                                    , Cmd.batch
                                        [ closeDialog ()
                                        , fetchLedgerLineSummary
                                        ]
                                    )

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
                                        , dateUnix = Time.posixToMillis data.date // 1000
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
                            case result of
                                Err _ ->
                                    ( model
                                      -- TODO: display toast error or similar
                                    , Cmd.none
                                    )

                                Ok _ ->
                                    ( { model
                                        | dialog = Nothing
                                      }
                                    , Cmd.batch
                                        [ closeDialog ()
                                        , fetchLedgerLineSummary
                                        ]
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


handleEditDialog : ( Int, TransactionWrite ) -> Model -> ( Model, Cmd Msg )
handleEditDialog ( transactionId, tx ) model =
    -- let
    --     dateString =
    --         Iso8601.fromTime tx.date
    --     hasTimeInfo =
    --         String.contains "T" dateString
    --             && not (String.endsWith "T00:00:00.000Z" dateString)
    --             && not (String.endsWith "T00:00:00Z" dateString)
    -- in
    let
        editDialogModel : MkEditDialog
        editDialogModel =
            { transactionId = transactionId
            , fromAccountId = tx.fromAccountId
            , toAccountId = tx.toAccountId
            , amount = String.fromInt tx.cents
            , date = Time.millisToPosix (tx.dateUnix * 1000)
            , descr = tx.descr
            , showTime = True -- FIXME: observe the unix ts trailing info
            }
    in
    ( { model
        | dialog =
            Just
                (EditDialog editDialogModel)
      }
    , showDialog ()
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
