module Page.Budget.Edit exposing (Model, Msg, init, update, view)

import Generated.Decoder exposing (decodeBudgetJSON)
import Generated.Types exposing (BudgetJSON)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as D
import Route
import Time exposing (Month(..))


type Status a
    = Loading
    | Failed
    | Loaded a


type alias Model =
    { budget : Status BudgetJSON
    }


fetchBudget : { id : Int } -> Cmd Msg
fetchBudget { id } =
    Http.get
        { url = "http://localhost:8080/budgets/" ++ String.fromInt id
        , expect = Http.expectJson GotBudgetResponse decodeBudgetJSON
        }


init : { id : Int } -> ( Model, Cmd Msg )
init { id } =
    ( { budget = Loading
      }
    , fetchBudget { id = 1 }
    )


type Msg
    = GotBudgetResponse (Result Http.Error BudgetJSON)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBudgetResponse response ->
            case response of
                Ok budget ->
                    ( { model | budget = Loaded budget }
                    , Cmd.none
                    )

                Err _ ->
                    -- TODO: handle error properly
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.budget of
        Loading ->
            H.div [] [ H.text "Loading..." ]

        Failed ->
            H.div [] [ H.text "Failed" ]

        Loaded budget ->
            H.text <|
                Debug.toString budget
