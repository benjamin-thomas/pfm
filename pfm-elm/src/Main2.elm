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
    , dialog : Maybe Dialog
    }


type Dialog
    = Dialog1 DialogForm1
    | Dialog2 DialogForm2


type alias DialogForm1 =
    { input1 : String
    , select1 : String
    , input2 : String
    , select2 : String
    }


type alias DialogForm2 =
    { name : String
    , email : String
    , comment : String
    }


emptyForm1 : DialogForm1
emptyForm1 =
    { input1 = ""
    , select1 = "option1"
    , input2 = ""
    , select2 = "option3"
    }


emptyForm2 : DialogForm2
emptyForm2 =
    { name = ""
    , email = ""
    , comment = ""
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
    = ShowDialog1
    | ShowDialog2
    | CloseDialog
    | Dialog1FormMsg Dialog1FormMsg
    | Dialog2FormMsg Dialog2FormMsg


type Dialog1FormMsg
    = Input1Changed String
    | Select1Changed String
    | Input2Changed String
    | Select2Changed String


type Dialog2FormMsg
    = NameChanged String
    | EmailChanged String
    | CommentChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowDialog1 ->
            ( { model | dialog = Just (Dialog1 emptyForm1) }
            , showDialog ()
            )

        ShowDialog2 ->
            ( { model | dialog = Just (Dialog2 emptyForm2) }
            , showDialog ()
            )

        CloseDialog ->
            ( { model | dialog = Nothing }
            , closeDialog ()
            )

        Dialog1FormMsg formMsg ->
            case model.dialog of
                Just (Dialog1 form) ->
                    ( { model | dialog = Just (Dialog1 (updateDialog1Form formMsg form)) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Dialog2FormMsg formMsg ->
            case model.dialog of
                Just (Dialog2 form) ->
                    ( { model | dialog = Just (Dialog2 (updateDialog2Form formMsg form)) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


updateDialog1Form : Dialog1FormMsg -> DialogForm1 -> DialogForm1
updateDialog1Form msg form =
    case msg of
        Input1Changed value ->
            { form | input1 = value }

        Select1Changed value ->
            { form | select1 = value }

        Input2Changed value ->
            { form | input2 = value }

        Select2Changed value ->
            { form | select2 = value }


updateDialog2Form : Dialog2FormMsg -> DialogForm2 -> DialogForm2
updateDialog2Form msg form =
    case msg of
        NameChanged value ->
            { form | name = value }

        EmailChanged value ->
            { form | email = value }

        CommentChanged value ->
            { form | comment = value }



-- VIEW


whenJust : Maybe a -> (a -> Html msg) -> Html msg
whenJust x f =
    Maybe.withDefault (H.text "") <| Maybe.map f x


view : Model -> Html Msg
view model =
    H.div []
        [ viewTable model.tableData
        , H.div [ HA.style "margin" "10px 0" ]
            [ H.button 
                [ HE.onClick ShowDialog1
                , HA.style "margin-right" "10px" 
                ] 
                [ H.text "Open Dialog 1" ]
            , H.button 
                [ HE.onClick ShowDialog2 ] 
                [ H.text "Open Dialog 2" ]
            ]
        , case model.dialog of
            Just (Dialog1 form) ->
                viewDialog1 form

            Just (Dialog2 form) ->
                viewDialog2 form

            Nothing ->
                H.text ""
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


viewDialog1 : DialogForm1 -> Html Msg
viewDialog1 form =
    H.node "dialog"
        [ HA.id "myDialog" ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [] [ H.text "Dialog 1 Form" ]
            , H.label [ HA.for "input1" ] [ H.text "Input 1:" ]
            , H.input [ HA.type_ "text", HA.id "input1", HE.onInput (Dialog1FormMsg << Input1Changed), HA.value form.input1 ] []
            , H.label [ HA.for "select1" ] [ H.text "Select 1:" ]
            , H.select [ HA.id "select1", HE.onInput (Dialog1FormMsg << Select1Changed) ]
                [ H.option [ HA.value "option1", HA.selected (form.select1 == "option1") ] [ H.text "Option 1" ]
                , H.option [ HA.value "option2", HA.selected (form.select1 == "option2") ] [ H.text "Option 2" ]
                ]
            , H.label [ HA.for "input2" ] [ H.text "Input 2 (Target Focus):" ]
            , H.input [ HA.type_ "text", HA.id "input2", HE.onInput (Dialog1FormMsg << Input2Changed), HA.value form.input2, HA.attribute "autofocus" "" ] []
            , H.label [ HA.for "select2" ] [ H.text "Select 2:" ]
            , H.select [ HA.id "select2", HE.onInput (Dialog1FormMsg << Select2Changed) ]
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


viewDialog2 : DialogForm2 -> Html Msg
viewDialog2 form =
    H.node "dialog"
        [ HA.id "myDialog" ]
        [ H.div [ HA.class "dialog-content" ]
            [ H.h3 [] [ H.text "Dialog 2 - Feedback Form" ]
            , H.label [ HA.for "name" ] [ H.text "Name:" ]
            , H.input [ HA.type_ "text", HA.id "name", HE.onInput (Dialog2FormMsg << NameChanged), HA.value form.name ] []
            , H.label [ HA.for "email" ] [ H.text "Email:" ]
            , H.input [ HA.type_ "email", HA.id "email", HE.onInput (Dialog2FormMsg << EmailChanged), HA.value form.email, HA.attribute "autofocus" "" ] []
            , H.label [ HA.for "comment" ] [ H.text "Comment:" ]
            , H.textarea 
                [ HA.id "comment"
                , HE.onInput (Dialog2FormMsg << CommentChanged)
                , HA.value form.comment
                , HA.style "width" "100%"
                , HA.style "min-height" "100px"
                ] []
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
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
