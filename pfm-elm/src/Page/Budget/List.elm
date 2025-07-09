module Page.Budget.List exposing (Model, Msg, init, update, view)

import Generated.Decoder exposing (decodeBudgetJSON)
import Generated.Types exposing (BudgetJSON)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Route
import Time exposing (Month(..))


type alias Model =
    { counter : Int
    , budgets : List BudgetJSON
    }


fetchBudgets : Cmd Msg
fetchBudgets =
    Http.get
        { url = "http://localhost:8080/budgets"
        , expect = Http.expectJson GotBudgetsResponse (D.list decodeBudgetJSON)
        }


init : ( Model, Cmd Msg )
init =
    ( { counter = 0
      , budgets = []
      }
    , fetchBudgets
    )


type Msg
    = GotBudgetsResponse (Result Http.Error (List BudgetJSON))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBudgetsResponse response ->
            case response of
                Ok budgets ->
                    ( { model | budgets = budgets }, Cmd.none )

                Err _ ->
                    -- TODO: handle error properly
                    ( model, Cmd.none )


formatDate : Int -> String
formatDate timestamp =
    let
        date =
            Time.millisToPosix (timestamp * 1000)

        month =
            case Time.toMonth Time.utc date of
                Jan ->
                    "Jan"

                Feb ->
                    "Feb"

                Mar ->
                    "Mar"

                Apr ->
                    "Apr"

                May ->
                    "May"

                Jun ->
                    "Jun"

                Jul ->
                    "Jul"

                Aug ->
                    "Aug"

                Sep ->
                    "Sep"

                Oct ->
                    "Oct"

                Nov ->
                    "Nov"

                Dec ->
                    "Dec"

        day =
            Time.toDay Time.utc date |> String.fromInt

        year =
            Time.toYear Time.utc date |> String.fromInt
    in
    month ++ " " ++ day ++ ", " ++ year


viewOneBudget : BudgetJSON -> Html msg
viewOneBudget budget =
    H.li [ HA.class "budget-item" ]
        [ H.div [ HA.class "budget-item__listing" ]
            [ H.div [ HA.class "budget-item__id" ]
                [ H.text ("Budget #" ++ String.fromInt budget.id) ]
            , H.div [ HA.class "budget-item__period" ]
                [ H.text (formatDate budget.startsOn ++ " - " ++ formatDate budget.endsOn) ]
            ]
        , H.div [ HA.class "budget-item__actions" ]
            [ H.a
                [ HA.class "button button--small"
                , Route.href (Route.BudgetEdit budget.id)
                ]
                [ H.text "Edit" ]
            ]
        ]


view : Model -> Html Msg
view model =
    H.div [ HA.class "container" ]
        [ H.div [ HA.class "page-header" ]
            [ H.div [ HA.class "page-header__title" ]
                [ H.h1 [] [ H.text "Budget Listing" ]
                ]
            , H.div [ HA.class "page-header__actions" ]
                [ H.a
                    [ Route.href Route.Home
                    , HA.class "button button--secondary"
                    ]
                    [ H.text "Go back" ]
                , H.button
                    [ HA.class "button button--primary" ]
                    [ H.text "Add Budget" ]
                ]
            ]
        , H.div [ HA.class "budget-list" ]
            [ H.div [ HA.class "budget-list__header" ]
                [ H.div [] [ H.text "Budget Listing" ]
                , H.div [] [ H.text "Actions" ]
                ]
            , H.ul [ HA.class "budget-list__items" ]
                (List.map viewOneBudget model.budgets)
            ]
        ]
