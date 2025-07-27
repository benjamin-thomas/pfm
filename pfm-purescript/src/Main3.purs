module Main3 where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as HQE
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as Event
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLElement (toEventTarget)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent as MouseEvent

type State =
  { showContextMenu :: Boolean
  , menuX :: Number
  , menuY :: Number
  , selectedAction :: String
  }

data Action
  = Initialize
  | ShowContextMenu Number Number
  | HideMenu
  | MenuItemClick String

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { showContextMenu: false
  , menuX: 0.0
  , menuY: 0.0
  , selectedAction: "No action selected yet"
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ $ HH.ClassName "container" ]
    [ HH.h1_ [ HH.text "Halogen Context Menu" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "demo-area"
        , HP.id "demo1"
        , HP.ref (H.RefLabel "demo1")
        ]
        [ HH.h3_ [ HH.text "Right-click here" ]
        , HH.p_ [ HH.text "Demo area 1" ]
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "selected-item" ]
        [ HH.text state.selectedAction ]
    , HH.div
        [ HP.class_ $ HH.ClassName "demo-area"
        , HP.id "demo2"
        , HP.ref (H.RefLabel "demo2")
        , HP.style "background-color: #f9f9f9;"
        ]
        [ HH.h3_ [ HH.text "Right-click here too" ]
        , HH.p_ [ HH.text "Demo area 2" ]
        ]
    , if state.showContextMenu then renderContextMenu state
      else HH.text ""
    ]

renderContextMenu :: forall m. State -> H.ComponentHTML Action () m
renderContextMenu state =
  HH.div
    [ HP.class_ $ HH.ClassName "context-menu show"
    , HP.style $ "left: " <> show state.menuX <> "px; top: " <> show state.menuY <> "px;"
    ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "context-menu-item"
        , HE.onClick \_ -> MenuItemClick "Copy"
        ]
        [ HH.text "Copy" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "context-menu-item"
        , HE.onClick \_ -> MenuItemClick "Paste"
        ]
        [ HH.text "Paste" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "context-menu-item"
        , HE.onClick \_ -> MenuItemClick "Edit"
        ]
        [ HH.text "Edit" ]
    , HH.div
        [ HP.class_ $ HH.ClassName "context-menu-item"
        , HE.onClick \_ -> MenuItemClick "Delete"
        ]
        [ HH.text "Delete" ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    win <- H.liftEffect window

    -- Setup context menu handler for demo areas
    maybeDemo1 <- H.getHTMLElementRef (H.RefLabel "demo1")
    maybeDemo2 <- H.getHTMLElementRef (H.RefLabel "demo2")

    case maybeDemo1, maybeDemo2 of
      Just demo1, Just demo2 -> do
        -- Add context menu listener to demo1
        contextMenuListener1 <- H.liftEffect $ eventListener \e -> do
          Event.preventDefault e
          pure unit

        H.liftEffect $ addEventListener
          (Event.EventType "contextmenu")
          contextMenuListener1
          false
          (toEventTarget demo1)

        -- Add context menu listener to demo2
        contextMenuListener2 <- H.liftEffect $ eventListener \e -> do
          Event.preventDefault e
          pure unit

        H.liftEffect $ addEventListener
          (Event.EventType "contextmenu")
          contextMenuListener2
          false
          (toEventTarget demo2)

        -- Subscribe to context menu events on demo areas
        void $ H.subscribe $ HQE.eventListener
          (Event.EventType "contextmenu")
          (toEventTarget demo1)
          \e -> case MouseEvent.fromEvent e of
            Just mouseEvent -> Just (ShowContextMenu (toNumber $ MouseEvent.clientX mouseEvent) (toNumber $ MouseEvent.clientY mouseEvent))
            Nothing -> Nothing

        void $ H.subscribe $ HQE.eventListener
          (Event.EventType "contextmenu")
          (toEventTarget demo2)
          \e -> case MouseEvent.fromEvent e of
            Just mouseEvent -> Just (ShowContextMenu (toNumber $ MouseEvent.clientX mouseEvent) (toNumber $ MouseEvent.clientY mouseEvent))
            Nothing -> Nothing

      _, _ -> pure unit

    -- Global click handler to close menu
    void $ H.subscribe $ HQE.eventListener
      (Event.EventType "click")
      (Window.toEventTarget win)
      (const $ Just HideMenu)

  ShowContextMenu x y -> do
    H.modify_ \s -> s { showContextMenu = true, menuX = x, menuY = y }

  HideMenu ->
    H.modify_ \s -> s { showContextMenu = false }

  MenuItemClick action -> do
    H.modify_ \s -> s { selectedAction = "Action: " <> action, showContextMenu = false }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body