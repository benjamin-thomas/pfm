module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Time


type alias Account =
    { name : String
    }


type Direction
    = Debit
    | Credit


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


book : List Transaction
book =
    [ { date = Time.millisToPosix 0
      , descr = "Opening balance"
      , from = openingBalance
      , to = checkingAccount
      , amount = 100000 -- 1000€
      }
    , { date = Time.millisToPosix 1
      , descr = "Groceries"
      , from = checkingAccount
      , to = spar
      , amount = 999 -- 9.99€
      }
    , { date = Time.millisToPosix 1
      , descr = "Book purchase"
      , from = checkingAccount
      , to = amazon
      , amount = 5499
      }
    , { date = Time.millisToPosix 2
      , descr = "Salary"
      , from = employerABC
      , to = checkingAccount
      , amount = 100000 -- 1000€
      }
    ]


balance : List Transaction -> Account -> Int
balance txs account =
    List.foldl
        (\{ from, to, amount } total ->
            if to == account then
                total + amount

            else if from == account then
                total - amount

            else
                total
        )
        0
        txs


type alias Model =
    { selectedAccountName : String
    }


type Msg
    = AccountSelected String



-- amountFmt : Int -> String
-- amountFmt amount =
--     let
--         str =
--             String.fromFloat
--                 (toFloat amount / 100)
--     in
--     str ++ " " ++ "€"


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
        found =
            Dict.get model.selectedAccountName allAccounts

        accountPadLen =
            60

        amountPadLen =
            10
    in
    H.div []
        [ H.pre [] [ H.text <| Debug.toString model ]
        , H.h2 []
            [ H.text "Select an account to compute the balance" ]
        , H.select
            [ HE.onInput AccountSelected
            ]
            (H.option [] [ H.text "none" ]
                :: List.map
                    (\o -> H.option [] [ H.text o.name ])
                    allAccounts_
            )
        , H.div [ HA.style "margin-top" "10px" ]
            [ case found of
                Just account ->
                    H.span []
                        [ H.text "Balance:\u{00A0}"
                        , H.text <| amountFmt <| balance book account
                        ]

                Nothing ->
                    H.text ""
            ]
        , H.div [ HA.style "margin-top" "10px" ]
            [ H.h3 [] [ H.text "All balances" ]
            , H.ul [ HA.style "font-family" "monospace" ]
                (List.map
                    (\o ->
                        H.li [ HA.style "list-style-type" "none" ]
                            [ H.text <| String.padRight accountPadLen '.' o.name
                            , H.text ":\u{00A0}"
                            , H.text <| String.padLeft amountPadLen '\u{00A0}' <| amountFmt <| balance book o
                            ]
                    )
                    allAccounts_
                )
            ]
        , H.div [ HA.style "margin-top" "10px" ]
            [ H.h3 [] [ H.text "Transactions" ]
            , H.ul [ HA.style "font-family" "monospace" ]
                (List.map
                    (\o ->
                        H.li [ HA.style "list-style-type" "none" ]
                            [ H.text <| String.padRight accountPadLen '.' o.descr
                            , H.text ":\u{00A0}"
                            , H.text <| String.padLeft amountPadLen '\u{00A0}' <| amountFmt o.amount
                            ]
                    )
                    book
                )
            ]
        ]


init : Model
init =
    { selectedAccountName = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AccountSelected str ->
            { model | selectedAccountName = str }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
