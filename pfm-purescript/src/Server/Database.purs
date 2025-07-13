module Server.Database
  ( initDatabase
  , getAllUsers
  , insertUser
  , deleteUser
  , seedDatabase
  ) where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import SQLite3 as SQLite3
import Shared.Types (User(..))

-- | Initialize the database and create tables
initDatabase :: String -> Aff SQLite3.DBConnection
initDatabase dbPath = do
  liftEffect $ log $ "Initializing database at: " <> dbPath
  db <- SQLite3.newDB dbPath
  createTables db
  pure db

-- | Create the users table
createTables :: SQLite3.DBConnection -> Aff Unit
createTables db = do
  liftEffect $ log "Creating tables..."
  _ <- SQLite3.queryDB db
    """
    CREATE TABLE IF NOT EXISTS users
      ( id INTEGER PRIMARY KEY
      , firstName TEXT NOT NULL
      , lastName TEXT NOT NULL
      , UNIQUE (firstName, lastName)
      )
  """
    []
  liftEffect $ log "Tables created successfully"

-- | Get all users from the database
getAllUsers :: SQLite3.DBConnection -> Aff (Array User)
getAllUsers db = do
  rows <- SQLite3.queryDB db "SELECT id, firstName, lastName FROM users" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  pure $ map rowToUser rowArray
  where
  rowToUser :: Foreign -> User
  rowToUser row =
    let
      obj = unsafeFromForeign row :: { id :: Int, firstName :: String, lastName :: String }
    in
      User { id: Just obj.id, firstName: obj.firstName, lastName: obj.lastName }

-- | Insert a new user into the database
insertUser :: User -> SQLite3.DBConnection -> Aff User
insertUser (User user) db = do
  _ <- SQLite3.queryDB db
    "INSERT INTO users (firstName, lastName) VALUES (?, ?)"
    [ unsafeToForeign user.firstName, unsafeToForeign user.lastName ]

  -- Get the newly created user
  rows <- SQLite3.queryDB db
    "SELECT id, firstName, lastName FROM users WHERE firstName = ? AND lastName = ? ORDER BY id DESC LIMIT 1"
    [ unsafeToForeign user.firstName, unsafeToForeign user.lastName ]

  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> pure $ User user -- fallback
    Just row ->
      let
        obj = unsafeFromForeign row :: { id :: Int, firstName :: String, lastName :: String }
      in
        pure $ User { id: Just obj.id, firstName: obj.firstName, lastName: obj.lastName }

-- | Delete a user by ID
deleteUser :: Int -> SQLite3.DBConnection -> Aff Unit
deleteUser userId db = do
  _ <- SQLite3.queryDB db "DELETE FROM users WHERE id = ?" [ unsafeToForeign userId ]
  pure unit

-- | Seed the database with initial data
seedDatabase :: SQLite3.DBConnection -> Aff Unit
seedDatabase db = do
  liftEffect $ log "Checking if database needs seeding..."
  rows <- SQLite3.queryDB db "SELECT COUNT(*) as count FROM users" []
  let rowArray = unsafeFromForeign rows :: Array Foreign
  case head rowArray of
    Nothing -> seed
    Just row ->
      let
        obj = unsafeFromForeign row :: { count :: Int }
      in
        if obj.count == 0 then seed else liftEffect $ log "Database already has data, skipping seed"
  where
  seed = do
    liftEffect $ log "Seeding database..."
    _ <- insertUser (User { id: Nothing, firstName: "John", lastName: "Doe" }) db
    _ <- insertUser (User { id: Nothing, firstName: "Jane", lastName: "Smith" }) db
    liftEffect $ log "Database seeded successfully"