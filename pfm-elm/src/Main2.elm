port module Main2 exposing (main)

import Browser
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE



-- PORTS


port showDialog : () -> Cmd msg


port closeDialog : () -> Cmd msg



-- MODEL


type alias Model =
    { tableData : List Person
    , dialog : Maybe DialogForm
    }


type alias DialogForm =
    { input1 : String
    , select1 : String
    , input2 : String
    , select2 : String
    }


emptyForm : DialogForm
emptyForm =
    { input1 = ""
    , select1 = "option1"
    , input2 = ""
    , select2 = "option3"
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
      , dialog = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ShowDialog
    | CloseDialog
    | DialogFormMsg DialogFormMsg


type DialogFormMsg
    = Input1Changed String
    | Select1Changed String
    | Input2Changed String
    | Select2Changed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowDialog ->
            ( { model | dialog = Just emptyForm }
            , showDialog ()
            )

        CloseDialog ->
            ( { model | dialog = Nothing }
            , closeDialog ()
            )

        DialogFormMsg formMsg ->
            case model.dialog of
                Just form ->
                    ( { model | dialog = Just (updateDialogForm formMsg form) }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


updateDialogForm : DialogFormMsg -> DialogForm -> DialogForm
updateDialogForm msg form =
    case msg of
        Input1Changed value ->
            { form | input1 = value }

        Select1Changed value ->
            { form | select1 = value }

        Input2Changed value ->
            { form | input2 = value }

        Select2Changed value ->
            { form | select2 = value }



-- VIEW


whenJust : Maybe a -> (a -> Html msg) -> Html msg
whenJust x f =
    Maybe.withDefault (H.text "") <| Maybe.map f x


view : Model -> Html Msg
view model =
    H.div []
        [ viewTable model.tableData
        , H.button [ HE.onClick ShowDialog ] [ H.text "Open Dialog" ]
        , whenJust model.dialog viewDialog
        ]


viewTable : List Person -> Html Msg
viewTable people =
    H.table []
        [ H.thead []
            [ H.tr []
                [ H.th [] [ H.text "Name" ]
                , H.th [] [ H.text "Age" ]
                ]
            ]
        , H.tbody [] (List.map viewTableRow people)
        ]


viewTableRow : Person -> Html Msg
viewTableRow person =
    H.tr []
        [ H.td [] [ H.text person.name ]
        , H.td [] [ H.text (String.fromInt person.age) ]
        ]


viewDialog : DialogForm -> Html Msg
viewDialog form =
    H.node "dialog"
        [ HA.id "myDialog" ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [] [ H.text "Dialog Form" ]
            , H.label [ HA.for "input1" ] [ H.text "Input 1:" ]
            , H.input [ HA.type_ "text", HA.id "input1", HE.onInput (DialogFormMsg << Input1Changed), HA.value form.input1 ] []
            , H.label [ HA.for "select1" ] [ H.text "Select 1:" ]
            , H.select [ HA.id "select1", HE.onInput (DialogFormMsg << Select1Changed) ]
                [ H.option [ HA.value "option1", HA.selected (form.select1 == "option1") ] [ H.text "Option 1" ]
                , H.option [ HA.value "option2", HA.selected (form.select1 == "option2") ] [ H.text "Option 2" ]
                ]
            , H.label [ HA.for "input2" ] [ H.text "Input 2 (Target Focus):" ]
            , H.input [ HA.type_ "text", HA.id "input2", HE.onInput (DialogFormMsg << Input2Changed), HA.value form.input2, HA.attribute "autofocus" "" ] []
            , H.label [ HA.for "select2" ] [ H.text "Select 2:" ]
            , H.select [ HA.id "select2", HE.onInput (DialogFormMsg << Select2Changed) ]
                [ H.option [ HA.value "option3", HA.selected (form.select2 == "option3") ] [ H.text "Option 3" ]
                , H.option [ HA.value "option4", HA.selected (form.select2 == "option4") ] [ H.text "Option 4" ]
                ]
            , H.button
                [ HA.id "closeDialog"
                , HE.onClick CloseDialog
                , HA.style "margin-top" "10px"
                ]
                [ H.text "Close" ]
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
