module Page.Budgets exposing (Model, Msg, init, update, view)

import Generated.Decoder exposing (decodeBudgetJSON)
import Generated.Types exposing (BudgetJSON)
import Html as H exposing (Html)
import Html.Events as HE
import Http
import Json.Decode as D
import Route


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
    = Inc
    | GotBudgetsResponse (Result Http.Error (List BudgetJSON))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Inc ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )

        GotBudgetsResponse response ->
            case response of
                Ok budgets ->
                    ( { model | budgets = budgets }, Cmd.none )

                Err _ ->
                    -- TODO: handle error properly
                    ( model, Cmd.none )


viewOneBudget : BudgetJSON -> Html msg
viewOneBudget budget =
    H.li [] [ H.text <| Debug.toString budget ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.text "hello budgets"
        , H.div []
            [ H.text "Counter: "
            , H.text (String.fromInt model.counter)
            , H.button
                [ HE.onClick Inc ]
                [ H.text "+" ]
            ]
        , H.div []
            [ H.a [ Route.href Route.Home ] [ H.text "Go back" ]
            ]
        , H.h2 [] [ H.text <| "Budgets " ++ String.fromInt (List.length model.budgets) ]
        , H.ul []
            (List.map viewOneBudget model.budgets)
        ]
