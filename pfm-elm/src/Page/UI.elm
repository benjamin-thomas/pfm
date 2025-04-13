module Page.UI exposing (view)

import Decimal exposing (Decimal)
import Domain exposing (TransactionViewWithBalance)
import Html as H exposing (Html)
import Html.Attributes as HA
import Time
import Utils exposing (amountFmt, formatDate)



-- TYPES


type alias BalanceCard =
    { category : String
    , account : String
    , amount : Decimal
    , colorAccent : String
    }


type ComponentToShow
    = TransactionListComponent
    | BalanceCardsComponent
    | ButtonsComponent
    | AllComponents



-- MAIN VIEW


view : Html msg
view =
    let
        -- Get the component from the URL query parameter
        urlParams =
            -- This is a placeholder, in a real app we would use Browser.Navigation
            -- For now, we'll use the hash for navigation
            ""

        activeComponent =
            if String.contains "component=balance-cards" urlParams then
                BalanceCardsComponent

            else if String.contains "component=transaction-list" urlParams then
                TransactionListComponent

            else if String.contains "component=buttons" urlParams then
                ButtonsComponent

            else
                AllComponents
    in
    H.div [ HA.class "ui-page" ]
        [ H.div [ HA.class "ui-page__content" ]
            [ viewSidebar activeComponent
            , viewComponentLibrary activeComponent
            ]
        ]



-- SIDEBAR NAVIGATION


viewSidebar : ComponentToShow -> Html msg
viewSidebar activeComponent =
    H.div [ HA.class "ui-sidebar" ]
        [ H.div [ HA.class "ui-sidebar__header" ]
            [ H.h1 [] [ H.text "Component Library" ]
            , H.a [ HA.href "/" ] [ H.text "Go back to root" ]
            ]
        , H.nav [ HA.class "ui-sidebar__nav" ]
            [ H.ul [ HA.class "ui-sidebar__nav-list" ]
                [ viewNavItem "All Components" "#all" (activeComponent == AllComponents)
                , viewNavItem "Balance Cards" "#balance-cards" (activeComponent == BalanceCardsComponent)
                , viewNavItem "Transaction List" "#transaction-list" (activeComponent == TransactionListComponent)
                , viewNavItem "Buttons" "#buttons" (activeComponent == ButtonsComponent)
                ]
            ]
        ]


viewNavItem : String -> String -> Bool -> Html msg
viewNavItem label href isActive =
    H.li []
        [ H.a
            [ HA.id (String.dropLeft 1 href)
            , HA.href href
            , HA.class "ui-sidebar__nav-item"
            , HA.classList [ ( "ui-sidebar__nav-item--active", isActive ) ]
            ]
            [ H.text label ]
        ]



-- COMPONENT LIBRARY


viewComponentLibrary : ComponentToShow -> Html msg
viewComponentLibrary componentToShow =
    H.div [ HA.class "ui-component-library" ]
        (case componentToShow of
            AllComponents ->
                [ viewComponentSection "Balance Cards" (viewBalanceCards sampleBalances)
                , viewComponentSection "Transaction List" (viewTransactionList sampleTransactions)
                , viewComponentSection "Buttons" viewButtons
                ]

            BalanceCardsComponent ->
                [ viewComponentSection "Balance Cards" (viewBalanceCards sampleBalances) ]

            TransactionListComponent ->
                [ viewComponentSection "Transaction List" (viewTransactionList sampleTransactions) ]

            ButtonsComponent ->
                [ viewComponentSection "Buttons" viewButtons ]
        )


viewComponentSection : String -> Html msg -> Html msg
viewComponentSection title content =
    H.section [ HA.class "ui-component-section" ]
        [ H.h2 [ HA.class "ui-component-section__title" ] [ H.text title ]
        , H.div [ HA.class "ui-component-section__content" ] [ content ]
        , H.div [ HA.class "ui-component-section__code" ]
            [ H.h3 [] [ H.text "Usage" ]
            , H.pre [ HA.class "ui-component-section__code-block" ]
                [ H.code []
                    [ H.text (getComponentUsageExample title) ]
                ]
            ]
        ]


getComponentUsageExample : String -> String
getComponentUsageExample componentName =
    case componentName of
        "Balance Cards" ->
            """viewBalanceCards : List BalanceCard -> Html msg
viewBalanceCards balances =
    H.div [ HA.class "balance-cards" ]
        (List.map viewBalanceCard balances)

viewBalanceCard : BalanceCard -> Html msg
viewBalanceCard balance =
    H.div 
        [ HA.class "balance-card"
        , HA.style "border-left-color" balance.colorAccent
        ]
        [ H.div [ HA.class "balance-card__category" ] 
            [ H.text balance.category ]
        , H.div [ HA.class "balance-card__account" ] 
            [ H.text balance.account ]
        , H.div [ HA.class "balance-card__amount" ] 
            [ H.text (formatAmount balance.amount) ]
        ]"""

        "Transaction List" ->
            """viewTransactionList : List ( Int, TransactionViewWithBalance ) -> Html msg
viewTransactionList transactions =
    H.div [ HA.class "transaction-list" ]
        [ H.div [ HA.class "transaction-list__header" ]
            [ H.h3 [] [ H.text "Transactions" ]
            , H.button [ HA.class "button button--primary" ]
                [ H.text "Add Transaction" ]
            ]
        , H.ul [ HA.class "transaction-list__items" ]
            (List.map viewTransactionItem transactions)
        ]"""

        "Buttons" ->
            """-- Primary Button
H.button [ HA.class "button button--primary" ] [ H.text "Primary Button" ]

-- Secondary Button
H.button [ HA.class "button button--secondary" ] [ H.text "Secondary Button" ]

-- Small Button
H.button [ HA.class "button button--small" ] [ H.text "Small Button" ]"""

        _ ->
            "-- No example available"



-- BALANCE CARDS COMPONENT


viewBalanceCards : List BalanceCard -> Html msg
viewBalanceCards balances =
    H.div [ HA.class "balance-cards" ]
        (List.map viewBalanceCard balances)


viewBalanceCard : BalanceCard -> Html msg
viewBalanceCard balance =
    H.div
        [ HA.class "balance-card"
        , HA.style "border-left-color" balance.colorAccent
        ]
        [ H.div [ HA.class "balance-card__category" ]
            [ H.text balance.category ]
        , H.div [ HA.class "balance-card__account" ]
            [ H.text balance.account ]
        , H.div [ HA.class "balance-card__amount" ]
            [ H.text (amountFmt balance.amount) ]
        ]



-- TRANSACTION LIST COMPONENT


viewTransactionList : List ( Int, TransactionViewWithBalance ) -> Html msg
viewTransactionList transactions =
    H.div [ HA.class "transaction-list" ]
        [ H.div [ HA.class "transaction-list__header" ]
            [ H.h3 [] [ H.text "Transactions" ]
            , H.button [ HA.class "button button--primary" ]
                [ H.text "Add Transaction" ]
            ]
        , H.ul [ HA.class "transaction-list__items" ]
            (List.map viewTransactionItem transactions)
        ]


viewTransactionItem : ( Int, TransactionViewWithBalance ) -> Html msg
viewTransactionItem ( id, transaction ) =
    let
        isPositive =
            Decimal.gt transaction.amount Decimal.zero

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
    H.li [ HA.class "transaction-item" ]
        [ H.div [ HA.class "transaction-item__details" ]
            [ H.div [ HA.class "transaction-item__description" ]
                [ H.text transaction.descr ]
            , H.div [ HA.class "transaction-item__accounts" ]
                [ H.text (transaction.from.name ++ " → " ++ transaction.to.name) ]
            ]
        , H.div [ HA.class "transaction-item__date" ]
            [ H.text (formatDate transaction.date) ]
        , H.div [ HA.class amountClass ]
            [ H.text (amountSign ++ amountFmt transaction.amount) ]
        ]



-- BUTTONS COMPONENT


viewButtons : Html msg
viewButtons =
    H.div [ HA.class "buttons-showcase" ]
        [ H.div [ HA.class "buttons-showcase__row" ]
            [ H.button [ HA.class "button" ] [ H.text "Default Button" ]
            , H.button [ HA.class "button button--primary" ] [ H.text "Primary Button" ]
            , H.button [ HA.class "button button--secondary" ] [ H.text "Secondary Button" ]
            ]
        , H.div [ HA.class "buttons-showcase__row" ]
            [ H.button [ HA.class "button button--small" ] [ H.text "Small Button" ]
            , H.button [ HA.class "button" ] [ H.text "Default Size" ]
            , H.button [ HA.class "button button--large" ] [ H.text "Large Button" ]
            ]
        ]



-- SAMPLE DATA


sampleBalances : List BalanceCard
sampleBalances =
    [ { category = "Assets"
      , account = "Checking Account"
      , amount = Decimal.fromString "998.60" |> Maybe.withDefault Decimal.zero
      , colorAccent = "#3498db"
      }
    , { category = "Assets"
      , account = "Savings Account"
      , amount = Decimal.fromString "0.00" |> Maybe.withDefault Decimal.zero
      , colorAccent = "#2ecc71"
      }
    , { category = "Expenses"
      , account = "Total Expenses"
      , amount = Decimal.fromString "102.40" |> Maybe.withDefault Decimal.zero
      , colorAccent = "#e74c3c"
      }
    ]


sampleTransactions : List ( Int, TransactionViewWithBalance )
sampleTransactions =
    [ ( 1
      , { date = Time.millisToPosix 1712332800000 -- April 6, 2024
        , descr = "Salary"
        , from = { name = "EmployerABC", category = { name = "Income" } }
        , to = { name = "Checking Account", category = { name = "Assets" } }
        , amount = Decimal.fromInt 1000
        , balanceMovement = { from = Decimal.fromInt 0, to = Decimal.fromInt 1000 }
        }
      )
    , ( 2
      , { date = Time.millisToPosix 1712246400000 -- April 5, 2024
        , descr = "Groceries"
        , from = { name = "Checking Account", category = { name = "Assets" } }
        , to = { name = "Spar", category = { name = "Expenses" } }
        , amount = Decimal.fromInt -50
        , balanceMovement = { from = Decimal.fromInt 1000, to = Decimal.fromInt 950 }
        }
      )
    , ( 3
      , { date = Time.millisToPosix 1712160000000 -- April 4, 2024
        , descr = "Book purchase"
        , from = { name = "Checking Account", category = { name = "Assets" } }
        , to = { name = "Amazon", category = { name = "Expenses" } }
        , amount = Decimal.fromInt -25
        , balanceMovement = { from = Decimal.fromInt 950, to = Decimal.fromInt 925 }
        }
      )
    ]
