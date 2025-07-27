module Main2 where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Web.DOM.Document (toNonElementParentNode, toNode) as Document
import Web.DOM.Element as Element
import Web.DOM.Element (toNode)
import Web.DOM.Node (toEventTarget)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event, EventType(..), preventDefault, stopPropagation)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as MouseEvent

foreign import setElementStyle :: String -> String -> Element.Element -> Effect Unit
foreign import addClassName :: String -> Element.Element -> Effect Unit
foreign import removeClassName :: String -> Element.Element -> Effect Unit
foreign import getClientX :: Event -> Number
foreign import getClientY :: Event -> Number

main :: Effect Unit
main = do
  win <- window
  doc <- document win
  let
    docNode = toDocument doc
    nonElementDoc = Document.toNonElementParentNode docNode

  -- Get elements
  maybeContextMenu <- getElementById "contextMenu" nonElementDoc
  maybeDemo1 <- getElementById "demo1" nonElementDoc
  maybeDemo2 <- getElementById "demo2" nonElementDoc
  maybeSelection <- getElementById "selection" nonElementDoc

  case maybeContextMenu, maybeDemo1, maybeDemo2, maybeSelection of
    Just contextMenu, Just demo1, Just demo2, Just selection -> do
      -- Create ref to track current target (unused for now in KISS implementation)
      _ <- Ref.new Nothing

      -- Context menu handler
      contextMenuListener <- eventListener \event -> do
        preventDefault event
        stopPropagation event
        let
          x = getClientX event
          y = getClientY event
        showContextMenu contextMenu x y

      -- Click handler to hide menu
      clickListener <- eventListener \_ -> do
        hideContextMenu contextMenu

      -- Menu item click handler
      menuClickListener <- eventListener \_ -> do
        -- For simplicity, just update selection text
        setElementTextContent selection "Action clicked!"
        hideContextMenu contextMenu

      -- Add event listeners
      let
        contextMenuEvent = EventType "contextmenu"
        clickEvent = EventType "click"
      addEventListener contextMenuEvent contextMenuListener false (toEventTarget $ toNode demo1)
      addEventListener contextMenuEvent contextMenuListener false (toEventTarget $ toNode demo2)
      addEventListener clickEvent clickListener false (toEventTarget $ Document.toNode docNode)
      addEventListener clickEvent menuClickListener false (toEventTarget $ toNode contextMenu)

    _, _, _, _ -> pure unit

showContextMenu :: Element.Element -> Number -> Number -> Effect Unit
showContextMenu menu x y = do
  setElementStyle "left" (show x <> "px") menu
  setElementStyle "top" (show y <> "px") menu
  addClassName "show" menu

hideContextMenu :: Element.Element -> Effect Unit
hideContextMenu menu = do
  removeClassName "show" menu

foreign import setElementTextContent :: Element.Element -> String -> Effect Unit