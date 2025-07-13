module Client.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = { count :: Int }

data Action = Inc | Dec

component :: forall q i o m. H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { count: 0 }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ HH.h1_ [ HH.text "PFM - PureScript" ]
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
    , HH.p_ [ HH.text "Minimal PureScript + Halogen app is working!" ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Inc -> H.modify_ \s -> s { count = s.count + 1 }
  Dec -> H.modify_ \s -> s { count = max 0 (s.count - 1) }

main :: Effect Unit
main = do
  log "[CLIENT] Booting up..."
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body