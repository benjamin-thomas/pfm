module Main2
  ( Dialog1FormMsg(..)
  , main
  ) where

import Prelude
import Data.Maybe (Maybe(..))
-- import Debug (spy)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

foreign import unsafeStringify :: forall a. a -> String

foreign import dialogShow :: String -> Effect Unit

foreign import dialogClose :: String -> Effect Unit

myDialog1Id :: String
myDialog1Id = "myDialog1"

myDialog2Id :: String
myDialog2Id = "myDialog2"

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

type Person
  = { name :: String
    , age :: Int
    }

-- derive instance Generic Person _
-- instance Show Person where
--   show = genericShow
type Dialog1
  = { input1 :: String
    , select1 :: String
    , input2 :: String
    , select2 :: String
    }

emptyForm1 :: Dialog1
emptyForm1 =
  { input1: ""
  , select1: ""
  , input2: ""
  , select2: ""
  }

type Dialog2
  = { name :: String
    , email :: String
    , comment :: String
    }

emptyForm2 :: Dialog2
emptyForm2 =
  { name: ""
  , email: ""
  , comment: ""
  }

data Dialog
  = MkDialog1 Dialog1
  | MkDialog2 Dialog2

-- derive instance Generic Dialog _
-- instance Show Dialog where
--   show = genericShow
type State
  = { tableData :: Array Person
    , dialog :: Maybe Dialog
    }

-- derive instance Generic State _
-- instance Show State where
--   show = genericShow
data Dialog1FormMsg
  = Input1Changed String
  | Select1Changed String
  | Input2Changed String
  | Select2Changed String

data Dialog2FormMsg
  = NameChanged String
  | EmailChanged String
  | CommentChanged String

data Action
  = ShowDialog1
  | ShowDialog2
  | CloseDialog { dialogId :: String }
  | Dialog1FormMsg Dialog1FormMsg
  | Dialog2FormMsg Dialog2FormMsg

component :: forall query output m. MonadEffect m => H.Component query Unit output m
component =
  H.mkComponent
    { initialState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    , render
    }
  where
  initialState :: Unit -> State
  initialState _ =
    { dialog: Nothing
    , tableData:
        [ { name: "John Doe"
          , age: 30
          }
        , { name: "Jane Smith"
          , age: 25
          }
        ]
    }

  handleDialog1FormMsg :: Dialog1FormMsg -> Dialog1 -> Dialog1
  handleDialog1FormMsg msg form = case msg of
    Input1Changed value -> form { input1 = value }
    Select1Changed value -> form { select1 = value }
    Input2Changed value -> form { input2 = value }
    Select2Changed value -> form { select2 = value }

  handleDialog2FormMsg :: Dialog2FormMsg -> Dialog2 -> Dialog2
  handleDialog2FormMsg msg form = case msg of
    NameChanged value -> form { name = value }
    EmailChanged value -> form { email = value }
    CommentChanged value -> form { comment = value }

  handleAction :: Action -> H.HalogenM State Action () output m Unit
  handleAction = case _ of
    ShowDialog1 -> do
      H.modify_ \state -> state { dialog = Just (MkDialog1 emptyForm1) }
      H.liftEffect $ dialogShow myDialog1Id
    ShowDialog2 -> do
      H.modify_ \state -> state { dialog = Just (MkDialog2 emptyForm2) }
      H.liftEffect $ dialogShow myDialog2Id
    CloseDialog { dialogId } -> do
      H.liftEffect $ dialogClose dialogId
      H.modify_ \state -> state { dialog = Nothing }
    Dialog1FormMsg msg ->
      H.modify_ \state ->
        state
          { dialog =
            case state.dialog of
              Just (MkDialog1 form) -> Just (MkDialog1 (handleDialog1FormMsg msg form))
              _ -> state.dialog
          }
    Dialog2FormMsg msg ->
      H.modify_ \state ->
        state
          { dialog =
            case state.dialog of
              Just (MkDialog2 form) -> Just (MkDialog2 (handleDialog2FormMsg msg form))
              _ -> state.dialog
          }

  render :: State -> H.ComponentHTML Action () m
  render state =
    -- let
    --   _ = spy "render" state
    -- in
    HH.div [ HP.id "app" ]
      [ HH.h1_ [ HH.text "Dialog handling example" ]
      -- , HH.pre
      --     [ HP.style "white-space: pre-wrap" ]
      --     [ HH.text $ unsafeStringify state ]
      , viewTable state.tableData
      , HH.div [ HP.style "margin: 10px 0" ]
          [ HH.button
              [ HE.onClick $ const ShowDialog1
              , HP.style "margin-right: 10px"
              ]
              [ HH.text "Open Dialog 1" ]
          , HH.button
              [ HE.onClick $ const ShowDialog2 ]
              [ HH.text "Open Dialog 2" ]
          ]
      , case state.dialog of
          Just (MkDialog1 form) -> viewDialog1 form
          Just (MkDialog2 form) -> viewDialog2 form
          Nothing -> HH.text ""
      ]

  viewDialog1 :: Dialog1 -> H.ComponentHTML Action () m
  viewDialog1 form =
    HH.dialog [ HP.id myDialog1Id ]
      [ HH.div [ HP.class_ $ HH.ClassName "dialog-content" ]
          [ HH.h3 [] [ HH.text "Dialog 1 Form" ]
          , HH.label [ HP.for "input1" ] [ HH.text "Input 1:" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id "input1"
              , HE.onValueInput (Dialog1FormMsg <<< Input1Changed)
              , HP.value form.input1
              ]
          , HH.label [ HP.for "select1" ] [ HH.text "Select 1:" ]
          , HH.select [ HP.id "select1", HE.onValueInput (Dialog1FormMsg <<< Select1Changed) ]
              [ HH.option [ HP.value "option1", HP.selected (form.select1 == "option1") ] [ HH.text "Option 1" ]
              , HH.option [ HP.value "option2", HP.selected (form.select1 == "option2") ] [ HH.text "Option 2" ]
              ]
          , HH.label [ HP.for "input2" ] [ HH.text "Input 2 (Target Focus):" ]
          , HH.input
              [ HP.type_ HP.InputText
              , HP.id "input2"
              , HE.onValueInput (Dialog1FormMsg <<< Input2Changed)
              , HP.value form.input2
              , HP.autofocus true
              ]
          , HH.label [ HP.for "select2" ] [ HH.text "Select 2:" ]
          , HH.select [ HP.id "select2", HE.onValueInput (Dialog1FormMsg <<< Select2Changed) ]
              [ HH.option [ HP.value "option3", HP.selected (form.select2 == "option3") ] [ HH.text "Option 3" ]
              , HH.option [ HP.value "option4", HP.selected (form.select2 == "option4") ] [ HH.text "Option 4" ]
              ]
          , HH.button
              [ HP.id "closeDialog"
              , HE.onClick $ const $ CloseDialog { dialogId: myDialog1Id }
              , HP.style "margin-top: 10px"
              ]
              [ HH.text "Close" ]
          ]
      ]

  viewDialog2 :: Dialog2 -> H.ComponentHTML Action () m
  viewDialog2 form =
    HH.dialog [ HP.id myDialog2Id ]
      [ HH.div [ HP.class_ $ HH.ClassName "dialog-content" ]
          [ HH.h3 [] [ HH.text "Dialog 2 - Feedback Form" ]
          , HH.label [ HP.for "name" ] [ HH.text "Name:" ]
          , Dialog2FormMsg
              <$> HH.input
                  [ HP.type_ HP.InputText
                  , HP.id "name"
                  , HE.onValueInput NameChanged
                  , HP.value form.name
                  ]
          , HH.label [ HP.for "email" ] [ HH.text "Email:" ]
          , Dialog2FormMsg
              <$> HH.input
                  [ HP.type_ HP.InputEmail
                  , HP.id "email"
                  , HE.onValueInput EmailChanged
                  , HP.value form.email
                  -- , HP.autofocus true
                  ]
          , HH.label [ HP.for "comment" ] [ HH.text "Comment:" ]
          , Dialog2FormMsg
              <$> HH.textarea
                  [ HP.id "comment"
                  , HE.onValueInput CommentChanged
                  , HP.value form.comment
                  , HP.style "width: 100%"
                  , HP.style "min-height: 100px"
                  ]
          , HH.br_
          , HH.button
              [ HP.id "closeDialog"
              , HE.onClick $ const $ CloseDialog { dialogId: myDialog2Id }
              , HP.style "margin-top: 10px"
              ]
              [ HH.text "Close" ]
          ]
      ]

  viewTable :: Array Person -> H.ComponentHTML Action () m
  viewTable people =
    HH.table [ HP.class_ $ HH.ClassName "table" ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "name" ]
              , HH.th_ [ HH.text "age" ]
              ]
          ]
      , HH.tbody_ $ map viewPerson people
      ]
    where
    viewPerson :: Person -> H.ComponentHTML Action () m
    viewPerson person =
      HH.tr_
        [ HH.td_ [ HH.text person.name ]
        , HH.td_ [ HH.text $ show person.age ]
        ]
