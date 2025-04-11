port module Main2 exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- PORTS


port showDialog : () -> Cmd msg


port closeDialog : () -> Cmd msg



-- MODEL


type alias Model =
    { tableData : List Person
    , dialogOpen : Bool
    }


type alias Person =
    { name : String
    , age : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tableData =
            [ { name = "John Doe", age = 30 }
            , { name = "Jane Smith", age = 25 }
            ]
      , dialogOpen = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = OpenDialog
    | CloseDialog
    | DialogClosed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenDialog ->
            ( { model | dialogOpen = True }
            , showDialog ()
            )

        CloseDialog ->
            ( model
            , closeDialog ()
            )

        DialogClosed ->
            ( { model | dialogOpen = False }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTable model.tableData
        , button [ onClick OpenDialog ] [ text "Open Dialog" ]
        , viewDialog
        ]


viewTable : List Person -> Html Msg
viewTable people =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Age" ]
                ]
            ]
        , tbody [] (List.map viewTableRow people)
        ]


viewTableRow : Person -> Html Msg
viewTableRow person =
    tr []
        [ td [] [ text person.name ]
        , td [] [ text (String.fromInt person.age) ]
        ]


viewDialog : Html Msg
viewDialog =
    node "dialog"
        [ id "myDialog" ]
        [ div [ class "dialog-content" ]
            [ h3 [] [ text "Dialog Form" ]
            , label [ for "input1" ] [ text "Input 1:" ]
            , input [ type_ "text", id "input1" ] []
            , label [ for "select1" ] [ text "Select 1:" ]
            , select [ id "select1" ]
                [ option [ value "option1" ] [ text "Option 1" ]
                , option [ value "option2" ] [ text "Option 2" ]
                ]
            , label [ for "input2" ] [ text "Input 2 (Target Focus):" ]
            , input [ type_ "text", id "input2", attribute "autofocus" "" ] []
            , label [ for "select2" ] [ text "Select 2:" ]
            , select [ id "select2" ]
                [ option [ value "option3" ] [ text "Option 3" ]
                , option [ value "option4" ] [ text "Option 4" ]
                ]
            , button
                [ id "closeDialog"
                , onClick CloseDialog
                , style "display" "block"
                , style "margin-top" "10px"
                ]
                [ text "Close" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
