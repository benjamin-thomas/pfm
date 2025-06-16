# SQL File Organization

## Current Approach

SQL queries are stored in separate `.sql` files located next to their consuming Haskell modules.

### File Structure

```
app/
  DB/
    User/
      getAll.sql              # SQL for getAllUserRows
      getNewPlatformUsers.sql # SQL for getNewPlatformUserRows
    User.hs                   # Haskell module that uses the SQL files
```

## How It Works

1. SQL queries are stored in separate `.sql` files next to their consuming Haskell modules
2. Files use camelCase naming to match Haskell naming conventions
3. The queries are embedded into the Haskell code using Template Haskell via the `file-embed` library
4. The embedded ByteString is converted to Text using `decodeUtf8` and wrapped in a `Query` constructor

## Usage Pattern

1. Create a new `.sql` file in the same directory as your Haskell module (or in a subdirectory)
2. Use camelCase for the filename to match Haskell naming conventions
3. In your Haskell module:
   - Add the necessary imports:
     ```haskell
     {-# LANGUAGE TemplateHaskell #-}
     
     import Data.FileEmbed (embedFile)
     import Data.Text.Encoding (decodeUtf8)
     import Database.SQLite.Simple (Query(Query))
     ```
   - Embed the SQL file in your function:
     ```haskell
     myQueryFunction :: Connection -> IO [ResultType]
     myQueryFunction conn = query_ conn sql :: IO [ResultType]
       where
         sql = Query (decodeUtf8 $(embedFile "app/Path/To/Module/myQuery.sql"))
     ```

## Benefits

- SQL queries get proper syntax highlighting in your editor
- Queries are embedded at compile-time, ensuring they exist and are valid
- Cleaner code with SQL logic separated from Haskell code
- SQL files are located close to their consumers, making navigation easier
- Consistent naming conventions with Haskell code
