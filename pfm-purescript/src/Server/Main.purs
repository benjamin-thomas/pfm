module Server.Main where

import Prelude hiding ((/))

import Effect (Effect)
import Effect.Console (log)
import HTTPurple (class Generic, Request, ResponseM, ServerM, RouteDuplex', mkRoute, noArgs, ok, serve)

data Route = Home

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  }

main :: Effect Unit
main = do
  log "[SERVER] Booting up..."
  void $ startServer

startServer :: ServerM
startServer = serve { port: 8080 } { route, router }

router :: Request Route -> ResponseM
router { route: Home } = ok "PFM PureScript Server is running!"