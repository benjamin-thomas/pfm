module Client.Main where

import Prelude

import Data.Array (mapWithIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Affjax.Web as AX
import Affjax.ResponseFormat as ResponseFormat
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Shared.Types (User(..))
import Yoga.JSON as JSON

type State = 
  { count :: Int
  , users :: Array User
  , loading :: Boolean
  , error :: Maybe String
  }

data Action 
  = Inc 
  | Dec 
  | LoadUsers

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> 
      { count: 0
      , users: []
      , loading: false
      , error: Nothing
      }
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just LoadUsers
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ HH.h1_ [ HH.text "PFM - PureScript" ]
    
    -- Counter section
    , HH.div
        [ HP.class_ (HH.ClassName "counter") ]
        [ HH.button
            [ HE.onClick \_ -> Dec
            , HP.disabled (state.count <= 0)
            ]
            [ HH.text "-" ]
        , HH.span_ [ HH.text $ " " <> show state.count <> " " ]
        , HH.button
            [ HE.onClick \_ -> Inc ]
            [ HH.text "+" ]
        ]
    
    -- Users section
    , HH.div_
        [ HH.h2_ [ HH.text "Users" ]
        , HH.button
            [ HE.onClick \_ -> LoadUsers
            , HP.disabled state.loading
            ]
            [ HH.text if state.loading then "Loading..." else "Refresh Users" ]
        , case state.error of
            Just error -> HH.div_ [ HH.text $ "Error: " <> error ]
            Nothing -> HH.text ""
        , HH.div_ $ mapWithIndex renderUser state.users
        ]
    ]
  where
  renderUser :: Int -> User -> H.ComponentHTML Action () m
  renderUser _ (User user) =
    HH.div_ 
      [ HH.text $ user.firstName <> " " <> user.lastName 
      , case user.id of
          Just id -> HH.text $ " (ID: " <> show id <> ")"
          Nothing -> HH.text ""
      ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Inc -> H.modify_ \s -> s { count = s.count + 1 }
  Dec -> H.modify_ \s -> s { count = max 0 (s.count - 1) }
  LoadUsers -> do
    H.modify_ \s -> s { loading = true, error = Nothing }
    result <- H.liftAff fetchUsers
    case result of
      Left error -> H.modify_ \s -> s { loading = false, error = Just error }
      Right users -> H.modify_ \s -> s { loading = false, users = users, error = Nothing }

fetchUsers :: Aff (Either String (Array User))
fetchUsers = do
  result <- AX.get ResponseFormat.string "http://localhost:8080/users"
  case result of
    Left err -> pure $ Left $ "Network error: " <> AX.printError err
    Right response ->
      case JSON.readJSON response.body of
        Left err -> pure $ Left $ "JSON decode error: " <> show err
        Right users -> pure $ Right users

main :: Effect Unit
main = do
  log "[CLIENT] Booting up..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body