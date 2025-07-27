module Main4 where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type State =
  { showContextMenu :: Boolean
  , menuX :: Number
  , menuY :: Number
  , selectedAction :: String
  }

data Action
  = Initialize
  | ShowContextMenu Event { soundexDescr :: String }
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
    [ HH.h1_ [ HH.text "Context Menu Test" ]
    , HH.div [ HP.class_ $ HH.ClassName "transaction-list" ]
        [ HH.h3_ [ HH.text "Transactions (right-click to test)" ]
        , HH.ul
            [ HP.class_ $ HH.ClassName "transaction-list__items" ]
            [ renderTransaction { id: 1, description: "Monthly Income", soundexDescr: "M534" }
            , renderTransaction { id: 2, description: "Rent Payment", soundexDescr: "R535" }
            , renderTransaction { id: 3, description: "Grocery Store", soundexDescr: "G626" }
            , renderTransaction { id: 4, description: "Gas Station", soundexDescr: "G234" }
            ]
        ]
    , if state.showContextMenu then renderContextMenu state else HH.text ""
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

renderTransaction
  :: forall m
   . { id :: Int, description :: String, soundexDescr :: String }
  -> H.ComponentHTML Action () m
renderTransaction txn =
  HH.li
    [ HP.class_ $ HH.ClassName "transaction-item"
    , HE.handler (EventType "contextmenu") \event -> do
        (ShowContextMenu event { soundexDescr: txn.soundexDescr })

    ]
    [ HH.div [ HP.class_ $ HH.ClassName "transaction-item__description" ]
        [ HH.text txn.description ]
    , HH.div [ HP.class_ $ HH.ClassName "transaction-item__soundex" ]
        [ HH.text $ "SOUNDEX: " <> txn.soundexDescr ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    pure unit

  ShowContextMenu evt { soundexDescr } -> do
    Console.log "ShowContextMenu 1"
    H.liftEffect $ Event.preventDefault evt
    case MouseEvent.fromEvent evt of
      Just mouseEvent -> do
        let pageX = toNumber $ MouseEvent.pageX mouseEvent
        let pageY = toNumber $ MouseEvent.pageY mouseEvent
        Console.logShow $ { soundexDescr, pageX, pageY }
        H.modify_ \s -> s { showContextMenu = true, menuX = pageX, menuY = pageY }
      Nothing -> do
        Console.error "Could not convert event to MouseEvent"

  HideMenu -> do
    H.modify_ \s -> s { showContextMenu = false }

  MenuItemClick action -> do
    H.modify_ \s -> s { selectedAction = "Action: " <> action, showContextMenu = false }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body