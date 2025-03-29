port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Time


port escapePressed : (() -> msg) -> Sub msg


type alias Account =
    { name : String
    }


type alias Transaction =
    { date : Time.Posix
    , descr : String
    , from : Account
    , to : Account
    , amount : Int -- in cents
    }


checkingAccount : Account
checkingAccount =
    { name = "Assets:Checking account" }


savingsAccount : Account
savingsAccount =
    { name = "Assets:Savings account" }


openingBalance : Account
openingBalance =
    { name = "Equity:OpeningBalance" }


employerABC : Account
employerABC =
    { name = "Income:EmployerABC" }


customerXYZ : Account
customerXYZ =
    { name = "Income:CustomerXYZ" }


spar : Account
spar =
    { name = "Expenses:Spar" }


amazon : Account
amazon =
    { name = "Expenses:Amazon" }


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


balance : Dict Int Transaction -> Account -> Int
balance txs account =
    Dict.foldl
        (\_ { from, to, amount } total ->
            if to == account then
                total + amount

            else if from == account then
                total - amount

            else
                total
        )
        0
        txs


type alias MkEditDialog =
    { transactionId : Int
    , descr : String
    , amount : String
    }


type Dialog
    = EditDialog MkEditDialog


type alias Model =
    { book : Dict Int Transaction
    , dialog : Maybe Dialog
    }


type MkEditDialogChanged
    = DescrChanged String
    | AmountChanged String
    | EditDialogSave


type Msg
    = TransactionClicked Int
    | EscapedPressed
    | EditDialogChanged MkEditDialogChanged


amountFmt : Int -> String
amountFmt amount =
    let
        euros =
            String.fromInt (amount // 100)

        cents =
            let
                n =
                    modBy 100 amount
            in
            String.padLeft 2 '0' (String.fromInt n)
    in
    String.concat
        [ euros
        , "."
        , cents
        , "\u{00A0}€"
        ]


view : Model -> Html Msg
view model =
    let
        accountPadLen =
            60

        amountPadLen =
            12
    in
    H.div []
        [ H.pre []
            [ H.text <|
                Debug.toString
                    { dialog = model.dialog }
            ]
        , H.div [ HA.style "margin-top" "10px" ]
            [ H.h3 [] [ H.text "All balances" ]
            , H.ul [ HA.class "entries" ]
                (List.map
                    (\o ->
                        H.li [ HA.class "entry" ]
                            [ H.text <| String.padRight accountPadLen '.' o.name
                            , H.text ":\u{00A0}"
                            , H.text <|
                                String.padLeft amountPadLen '\u{00A0}' <|
                                    amountFmt <|
                                        balance model.book o
                            ]
                    )
                    allAccounts_
                )
            ]
        , H.div [ HA.style "margin-top" "10px" ]
            [ H.h3 [] [ H.text "Transactions" ]
            , H.ul [ HA.class "entries" ]
                (List.map
                    (\( transactionId, o ) ->
                        H.li
                            [ HA.class "entry tx"
                            , HE.onClick (TransactionClicked transactionId)
                            ]
                            [ H.text <| String.padRight accountPadLen '.' o.descr
                            , H.text ":\u{00A0}"
                            , H.text <| String.padLeft amountPadLen '\u{00A0}' <| amountFmt o.amount
                            ]
                    )
                    (Dict.toList model.book)
                )
            ]
        , case model.dialog of
            Nothing ->
                H.text ""

            Just (EditDialog data) ->
                H.node "dialog"
                    [ HA.attribute "open" ""
                    ]
                    [ viewEditDialog data ]
        ]


viewEditDialog : MkEditDialog -> Html Msg
viewEditDialog data =
    let
        field msg text value =
            H.div []
                [ H.label []
                    [ H.text text
                    , H.input
                        [ HA.value value
                        , HE.onInput (EditDialogChanged << msg)
                        ]
                        []
                    ]
                ]
    in
    H.div []
        [ H.h3 [] [ H.text "Edit transaction" ]
        , field DescrChanged "Description" data.descr
        , field AmountChanged "Amount" data.amount
        , H.button
            [ HE.onClick EscapedPressed
            ]
            [ H.text "Cancel" ]
        , H.button
            [ HE.onClick (EditDialogChanged EditDialogSave)
            ]
            [ H.text "Save" ]
        ]


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        book : Dict Int Transaction
        book =
            Dict.fromList <|
                [ ( 1
                  , { date = Time.millisToPosix 0
                    , descr = "Opening balance"
                    , from = openingBalance
                    , to = checkingAccount
                    , amount = 100000 -- 1000€
                    }
                  )
                , ( 2
                  , { date = Time.millisToPosix 1
                    , descr = "Groceries"
                    , from = checkingAccount
                    , to = spar
                    , amount = 999 -- 9.99€
                    }
                  )
                , ( 3
                  , { date = Time.millisToPosix 1
                    , descr = "Book purchase"
                    , from = checkingAccount
                    , to = amazon
                    , amount = 5499
                    }
                  )
                , ( 4
                  , { date = Time.millisToPosix 2
                    , descr = "Salary"
                    , from = employerABC
                    , to = checkingAccount
                    , amount = 100000 -- 1000€
                    }
                  )
                ]
    in
    ( { dialog = Nothing
      , book = book
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TransactionClicked transactionId ->
            ( case Dict.get transactionId model.book of
                Just tx ->
                    { model
                        | dialog =
                            Just <|
                                EditDialog
                                    { transactionId = transactionId
                                    , descr = tx.descr
                                    , amount = String.fromInt tx.amount
                                    }
                    }

                Nothing ->
                    model
            , Cmd.none
            )

        EscapedPressed ->
            ( { model | dialog = Nothing }
            , Cmd.none
            )

        EditDialogChanged subMsg ->
            case model.dialog of
                Just (EditDialog data) ->
                    case subMsg of
                        DescrChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | descr = str } }
                            , Cmd.none
                            )

                        AmountChanged str ->
                            ( { model | dialog = Just <| EditDialog { data | amount = str } }
                            , Cmd.none
                            )

                        EditDialogSave ->
                            let
                                newBook =
                                    Dict.update data.transactionId
                                        (Maybe.andThen
                                            (\old ->
                                                Just
                                                    { old
                                                        | descr = data.descr
                                                        , amount = Maybe.withDefault old.amount (String.toInt data.amount)
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    escapePressed (\() -> EscapedPressed)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
