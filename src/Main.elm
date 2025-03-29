module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Time


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


type Dialog
    = EditDialog Transaction


type alias Model =
    { book : Dict Int Transaction
    , dialog : Maybe Dialog
    }


type Msg
    = TransactionClicked Int


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
        , H.node "dialog"
            (List.filterMap identity
                [ model.dialog |> Maybe.map (\_ -> HA.attribute "open" "")
                ]
            )
            [ H.text "wat" ]
        ]


init : Model
init =
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
    { dialog = Nothing
    , book = book
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TransactionClicked transactionId ->
            case Dict.get transactionId model.book of
                Just tx ->
                    { model | dialog = Just <| EditDialog tx }

                Nothing ->
                    model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
