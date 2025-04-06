port module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Decimal exposing (Decimal, zero)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Iso8601
import Page.UI as UI_Page
import Route exposing (Route)
import Set
import Task
import Time
import Url exposing (Url)


port escapePressed : (() -> msg) -> Sub msg


port enterPressed : (() -> msg) -> Sub msg


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


type alias TransactionViewWithBalance =
    { date : Time.Posix
    , descr : String
    , from : Account
    , to : Account
    , amount : Decimal
    , balanceMovement : { from : Decimal, to : Decimal }
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
    , amount : String
    }


type alias MkCreateDialog =
    { descr : String
    , amount : String
    }


type Dialog
    = EditDialog MkEditDialog
    | CreateDialog MkCreateDialog


type Page
    = NotFound
    | Home
    | UI


type alias Model =
    { key : Nav.Key
    , url : Url
    , route : Route
    , now : Time.Posix
    , zone : Time.Zone
    , book : Dict Int TransactionView
    , dialog : Maybe Dialog
    }


type MkEditDialogChanged
    = EditDescrChanged String
    | EditAmountChanged String
    | EditDialogSave


type MkCreateDialogChanged
    = CreateDescrChanged String
    | CreateAmountChanged String
    | CreateDialogSave


type Msg
    = UrlRequested UrlRequest
    | UrlChanged Url
    | TransactionClicked Int
    | EditDialogChanged MkEditDialogChanged
    | CreateDialogChanged MkCreateDialogChanged
    | AddTransactionClicked
    | EscapedPressed
    | EnterPressed
    | GotTime Time.Posix
    | GotZone Time.Zone


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


view : Model -> Browser.Document Msg
view model =
    let
        viewPage : Html Msg
        viewPage =
            case model.route of
                Route.NotFound ->
                    H.text "Not found"

                Route.Home ->
                    viewHome model

                Route.UI ->
                    UI_Page.view

        cssLink =
            case model.route of
                Route.UI ->
                    H.node "link"
                        [ HA.rel "stylesheet"
                        , HA.href "/ui.css"
                        ]
                        []

                _ ->
                    H.node "link"
                        [ HA.rel "stylesheet"
                        , HA.href "/main.css"
                        ]
                        []
    in
    { title = "Personal Finance Manager"
    , body = 
        [ cssLink
        , viewPage 
        ]
    }


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
                                    accountBalance = balance model.book account
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
                    allCategories
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
                                    Decimal.gt tx.amount Decimal.zero

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
                                , HE.onClick (TransactionClicked transactionId)
                                ]
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
                        )
                        transactions2
                    )
                ]
            ]
        , case model.dialog of
            Nothing ->
                H.text ""

            Just (EditDialog data) ->
                H.node "dialog"
                    [ HA.attribute "open" ""
                    ]
                    [ viewEditDialog data ]

            Just (CreateDialog data) ->
                H.node "dialog"
                    [ HA.attribute "open" ""
                    ]
                    [ viewCreateDialog data ]
        ]


field : { a | onInput : String -> msg, text : String, value : String } -> Html msg
field { onInput, text, value } =
    H.div [ HA.class "field" ]
        [ H.label []
            [ H.text text ]
        , H.input
            [ HA.value value
            , HE.onInput onInput
            ]
            []
        ]


viewDialog : { a | title : String, saveMsg : Msg, fields : List (Html Msg) } -> Html Msg
viewDialog { title, saveMsg, fields } =
    H.div [ HA.style "margin-bottom" "20px" ]
        (List.concat
            [ [ H.h3 [] [ H.text title ] ]
            , fields
            , [ H.div [ HA.style "margin-top" "30px" ]
                    [ H.button
                        [ HE.onClick EscapedPressed
                        ]
                        [ H.text "Cancel" ]
                    , H.button
                        [ HE.onClick saveMsg
                        ]
                        [ H.text "Save" ]
                    ]
              ]
            ]
        )


viewEditDialog : MkEditDialog -> Html Msg
viewEditDialog data =
    H.div []
        [ H.h3 [] [ H.text "Edit Transaction" ]
        , field
            { text = "Description"
            , value = data.descr
            , onInput = \str -> EditDialogChanged (EditDescrChanged str)
            }
        , field
            { text = "Amount"
            , value = data.amount
            , onInput = \str -> EditDialogChanged (EditAmountChanged str)
            }
        , H.div [ HA.class "actions" ]
            [ H.button
                [ HA.class "button"
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


viewCreateDialog : MkCreateDialog -> Html Msg
viewCreateDialog data =
    H.div []
        [ H.h3 [] [ H.text "Add Transaction" ]
        , field
            { text = "Description"
            , value = data.descr
            , onInput = \str -> CreateDialogChanged (CreateDescrChanged str)
            }
        , field
            { text = "Amount"
            , value = data.amount
            , onInput = \str -> CreateDialogChanged (CreateAmountChanged str)
            }
        , H.div [ HA.class "actions" ]
            [ H.button
                [ HA.class "button"
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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        v : String -> Decimal
        v =
            Maybe.withDefault zero << Decimal.fromString

        book : Dict Int TransactionView
        book =
            Dict.fromList <|
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
      , dialog = Nothing
      , book = book
      , now = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Task.perform GotZone Time.here
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        TransactionClicked transactionId ->
            ( case Dict.get transactionId model.book of
                Just tx ->
                    { model
                        | dialog =
                            Just <|
                                EditDialog
                                    { transactionId = transactionId
                                    , descr = tx.descr
                                    , amount = Decimal.toString tx.amount
                                    }
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        EditDialogChanged subMsg ->
            case model.dialog of
                Just (EditDialog data) ->
                    case subMsg of
                        EditDescrChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | descr = str } }
                            , Cmd.none
                            )

                        EditAmountChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | amount = str } }
                            , Cmd.none
                            )

                        EditDialogSave ->
                            let
                                newBook : Dict Int TransactionView
                                newBook =
                                    Dict.update data.transactionId
                                        (Maybe.andThen
                                            (\old ->
                                                Just
                                                    { old
                                                        | descr = data.descr
                                                        , amount =
                                                            Maybe.withDefault
                                                                old.amount
                                                                (Decimal.fromString data.amount)
                                                    }
                                            )
                                        )
                                        model.book
                            in
                            ( { model
                                | dialog = Nothing
                                , book = newBook
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model
                    , Cmd.none
                    )

        AddTransactionClicked ->
            ( { model
                | dialog =
                    Just <|
                        CreateDialog
                            { descr = ""
                            , amount = ""
                            }
              }
            , Cmd.none
            )

        CreateDialogChanged subMsg ->
            case model.dialog of
                Just (CreateDialog data) ->
                    case subMsg of
                        CreateDescrChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | descr = str } }
                            , Cmd.none
                            )

                        CreateAmountChanged str ->
                            ( { model | dialog = Just <| CreateDialog { data | amount = str } }
                            , Cmd.none
                            )

                        CreateDialogSave ->
                            let
                                newTransaction : TransactionView
                                newTransaction =
                                    { date = model.now
                                    , descr = data.descr
                                    , from = checkingAccount
                                    , to = spar
                                    , amount = Maybe.withDefault zero <| Decimal.fromString data.amount
                                    }

                                nextId : Int
                                nextId =
                                    Dict.keys model.book
                                        |> List.maximum
                                        |> Maybe.map ((+) 1)
                                        |> Maybe.withDefault 0

                                newBook : Dict Int TransactionView
                                newBook =
                                    Dict.insert nextId
                                        newTransaction
                                        model.book
                            in
                            ( { model
                                | dialog = Nothing
                                , book = newBook
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ escapePressed (\() -> EscapedPressed)
        , enterPressed (\() -> EnterPressed)
        , Time.every (Debug.log "time" 9991000) GotTime
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
