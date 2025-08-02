# PFM Clojure POC

## Context
This is a TDD proof-of-concept rewrite of a PureScript Personal Finance Manager in Clojure, following patterns from "Web Development with Clojure" book.

Make sure to update this file as we go along, in order to keep it up to date.

## Tech Stack
- **Backend**: Ring + Reitit routing + next.jdbc + SQLite
- **Testing**: Kaocha test runner
- **Build**: deps.edn (tools.deps)

## TDD Workflow
We're following strict TDD:
1. Write simplest failing test
2. User validates test
3. Write simplest code to pass
4. User validates implementation
5. Repeat

## Naming Convention
**IMPORTANT**: Follow Cursive toggle convention:
- Function: `my-func` 
- Test: `my-func-test`
This enables Ctrl+Shift+T toggling between function and test.

## Project Structure
```
src/pfm/
├── handler.clj    # Web handlers (health-check, app)
└── db.clj         # Database functions (get-connection, query)

test/pfm/
├── handler_test.clj  # Web handler tests
└── db_test.clj       # Database tests
```

## What's Implemented
✅ Basic web server with health check endpoint  
✅ Database connection (SQLite)  
✅ Basic SQL query execution (tested with 1+2=3)  

## Next Steps
1. Create database table + migration system
2. Serve transaction data as JSON
3. Add ClojureScript frontend
4. Connect frontend to backend

## Running Tests
- Command line: `clojure -M:test`
- Watch mode: `clojure -M:test --watch`
- IntelliJ: Right-click function → "Run test" (fastest feedback)

## Database
- **Test DB**: `db.test.sqlite` (isolated for tests)
- **Dev DB**: `db.test` (for development)
- Uses next.jdbc for queries
- **IMPORTANT**: Test and dev databases must be separate to avoid test pollution