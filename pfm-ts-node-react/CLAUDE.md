# PFM TypeScript Node React Project

## Project Overview
A fullstack TypeScript application for Personal Finance Management (PFM) that prioritizes type safety, simplicity, and functional programming principles.

## Architecture Principles

### Functional Programming Style
- **No unnecessary classes**: Use functions and closures instead of classes (unless they provide particular benefits)
- **Pure functions**: Prefer pure functions with explicit dependencies (unless not appropriate, think hot loop)
- **Immutability**: Favor immutable data structures (maybe later? we don't have the appropriate data structures by default in JS/TS)
- **Composition**: Build complex behavior by composing simple functions
- **Manual dependency injection**: Pass dependencies as function parameters (only at the "big" entrypoints, we can build "environments" via closures or classes to avoid repeating ourselves over and over for no real benefit)

### Repository Pattern
- Define interfaces for all data access (e.g., `TransactionRepo`, `AccountRepo`)
- Create fake implementations for testing (`TransactionRepoFake`)
- Create SQL implementations for production (`TransactionRepoSql`)
- Test against interfaces, not implementations

### Test-First Development
- Write interface definitions first
- Create fake implementations for testing
- Write tests against the interface
- Then implement the real (SQL) version
- Run same tests against both implementations
- Write one test a time, red green refactor, always

## Tech Stack
- **Frontend**: React + Vite + TypeScript (strict mode)
- **Backend**: Node.js HTTP server (no Express)
- **Database**: SQLite with Kysely (type-safe SQL, not an ORM)
- **Shared**: Common types between frontend and backend
- **Testing**: Vitest with fake repositories

## Port Configuration
- **Development**: Frontend 4003, Backend 8083
- **Test Environment**: Frontend 4004, Backend 8084

## Project Structure
```
pfm-ts-node-react/
├── front/          # React frontend
├── server/         # Node.js backend
│   ├── repos/      # Repository implementations
│   ├── services/   # Extra logic that doesn't fit in the simple Repo pattern (merging HTTP data + DB data for instance)
│   └── handlers/   # HTTP request handlers
├── shared/         # Shared types/interfaces
├── sql/            # SQL schemas and migrations
└── tests/          # Test files
```

## Code Style Guidelines

### TypeScript Configuration
- Always use strict mode
- Enable all strict flags in tsconfig.json
- No implicit any
- Explicit return types for public functions
- Encode/decode our data, always
  - maybe use: https://github.com/lydell/tiny-decoders
  - or a combination of zod+trpc?

### Function Style
```typescript
// Prefer this (FP style with closure, principle of least power)
const createTransactionRepo = (db: Database): TransactionRepo => {
  return {
    list: async (budgetId) => { /* ... */ },
    create: async (tx) => { /* ... */ }
  }
}

// Avoid this (class-based)
class TransactionRepository {
  constructor(private db: Database) {}
  async list(budgetId: number) { /* ... */ }
}
```

### Dependency Injection
```typescript
// Handler with injected dependencies
const createTransactionHandler = (repo: TransactionRepo) => {
  return async (req: Request, res: Response) => {
    const transactions = await repo.list(budgetId)
    // ...
  }
}

// Wire up in main
const repo = createTransactionRepoSql(db)
const handler = createTransactionHandler(repo)
```

## SQL Guidelines
- Use raw SQL queries via Kysely
- No ORMs or heavy abstractions
- Type safety through Kysely's schema types
- Keep queries simple and readable

## SSE (Server-Sent Events)
- Native implementation, no libraries
- Auto-reconnect on disconnection
- Separate SSE endpoints for different event types

## Development Commands
- `npm run dev:front` - Start frontend dev server (4003)
- `npm run dev:server` - Start backend dev server (8083)
- `npm run test:front` - Start frontend test server (4004)
- `npm run test:server` - Start backend test server (8084)
- `npm run test` - Run unit tests
- `npm run typecheck` - Check types
- `npm run lint` - Run linter

## Important Notes
- KISS principle: Keep It Simple, Stupid
- Avoid premature abstractions
- Start with fake repos for rapid prototyping
- Only add dependencies when absolutely necessary
- Prefer Node.js built-in modules over external packages