module Server.Main where

import Prelude hiding ((/))

import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import HTTPurple (class Generic, Method(..), Request, ResponseM, ServerM, RouteDuplex', header, int, methodNotAllowed, mkRoute, noArgs, ok, segment, serve, (/))
import SQLite3 (DBConnection)
import Server.Database (initDatabase, getAllUsers, insertUser, deleteUser, seedDatabase)
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
  initDatabase "./db.sqlite" # runAff_
    case _ of
      Left err -> log $ "Failed to initialize database: " <> show err
      Right db -> do
        log "Database initialized successfully"
        seedDatabase db # runAff_
          case _ of
            Left err -> log $ "Failed to seed database: " <> show err
            Right _ -> do
              log "Database seeded successfully"
              void $ startServer db

startServer :: DBConnection -> ServerM
startServer db = serve { port: 8080 } { route, router: corsMiddleware (makeRouter db) }

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

makeRouter :: DBConnection -> Request Route -> ResponseM
makeRouter db { route: route', method } =
  case route' of
    Home -> 
      case method of
        Get -> ok "PFM PureScript Server is running!"
        _ -> methodNotAllowed

    Users ->
      case method of
        Get -> do
          users <- getAllUsers db
          ok $ JSON.writeJSON users
        Post -> do
          -- For now, just create a test user
          newUser <- insertUser (User { id: Nothing, firstName: "New", lastName: "User" }) db
          ok $ JSON.writeJSON newUser
        _ -> methodNotAllowed

    UserById userId ->
      case method of
        Get -> ok $ JSON.writeJSON $ User { id: Just userId, firstName: "User", lastName: show userId }
        Delete -> do
          deleteUser userId db
          ok "User deleted"
        _ -> methodNotAllowed