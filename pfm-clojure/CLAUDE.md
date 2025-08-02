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

**Test Organization**: Use multiple `testing` blocks within a single `deftest` to keep related tests together while maintaining the toggle functionality:
```clojure
(deftest my-func-test
  (testing "basic scenario"
    (is ...))
  (testing "advanced scenario"
    (is ...)))
```

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
- **Parallel execution**: Enabled via `kaocha.testable/parallel? true` in tests.edn

## Database
- **Test DB**: Unique per test (e.g., `db.test.1234567-8901.sqlite`)
- **Dev DB**: `db.dev.sqlite` (for development)
- Uses next.jdbc for queries
- **Database isolation**: Each test gets its own database file with automatic cleanup
- **Parallel-safe**: Unique database files prevent conflicts between parallel tests