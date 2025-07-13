module Server.Main where

import Prelude hiding ((/))

import Data.Array ((:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import HTTPurple (class Generic, Method(..), Request, Response, ResponseM, ServerM, RouteDuplex', header, int, methodNotAllowed, mkRoute, noArgs, ok, segment, serve, (/))
import Shared.Types (User(..))
import Yoga.JSON as JSON

data Route
  = Home
  | Users
  | UserById Int

derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Home": noArgs
  , "Users": "users" / noArgs
  , "UserById": "users" / int segment
  }

main :: Effect Unit
main = do
  log "[SERVER] Booting up..."
  void $ startServer

startServer :: ServerM
startServer = serve { port: 8080 } { route, router: corsMiddleware router }

corsMiddleware :: (Request Route -> ResponseM) -> Request Route -> ResponseM
corsMiddleware router' request = do
  response <- router' request
  pure $ response
    { headers = foldl (<>) response.headers
        [ header "Access-Control-Allow-Origin" "*"
        , header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
        , header "Access-Control-Allow-Headers" "Content-Type, Authorization"
        ]
    }

router :: Request Route -> ResponseM
router { route: Home, method } = 
  case method of
    Get -> ok "PFM PureScript Server is running!"
    _ -> methodNotAllowed

router { route: Users, method } =
  case method of
    Get -> ok $ JSON.writeJSON mockUsers
    Post -> ok $ JSON.writeJSON $ User { id: Just 3, firstName: "New", lastName: "User" }
    _ -> methodNotAllowed

router { route: UserById userId, method } =
  case method of
    Get -> ok $ JSON.writeJSON $ User { id: Just userId, firstName: "User", lastName: show userId }
    Put -> ok $ JSON.writeJSON $ User { id: Just userId, firstName: "Updated", lastName: "User" }
    Delete -> ok "User deleted"
    _ -> methodNotAllowed

mockUsers :: Array User
mockUsers = 
  [ User { id: Just 1, firstName: "John", lastName: "Doe" }
  , User { id: Just 2, firstName: "Jane", lastName: "Smith" }
  ]