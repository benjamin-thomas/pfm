# TODO - PFM TypeScript Rewrite

## Phase 1: Foundation ✅
- [x] Create CLAUDE.md with FP principles
- [ ] Create TODO.md (this file)
- [ ] Copy SQL schema from PureScript project
- [ ] Copy main.css styles
- [ ] Setup TypeScript configurations (strict mode)
- [ ] Configure build scripts for ports 4003/8083 (dev) and 4004/8084 (test)

## Phase 2: Shared Types & Interfaces
- [ ] Create shared/types.ts with database entities
- [ ] Define repository interfaces (TransactionRepo, AccountRepo, etc.)
- [ ] Create shared DTOs for API communication
- [ ] Setup TypeScript path aliases for clean imports

## Phase 3: Test-First Repository Pattern
- [ ] Implement TransactionRepoFake with in-memory storage
- [ ] Write repository interface tests
- [ ] Implement AccountRepoFake
- [ ] Implement CategoryRepoFake
- [ ] Implement BudgetRepoFake

## Phase 4: Database Layer
- [ ] Setup SQLite with better-sqlite3
- [ ] Configure Kysely with type-safe schema
- [ ] Implement TransactionRepoSql
- [ ] Implement AccountRepoSql
- [ ] Implement CategoryRepoSql
- [ ] Implement BudgetRepoSql
- [ ] Run same tests against SQL implementations

## Phase 5: HTTP Server
- [ ] Create basic Node.js HTTP server (no Express)
- [ ] Implement request routing
- [ ] Add JSON parsing middleware
- [ ] Setup CORS for frontend
- [ ] Create health check endpoint
- [ ] Support APP_ENV for dev/test environments

## Phase 6: API Handlers
- [ ] Create transaction list handler
- [ ] Create transaction create handler
- [ ] Create account handlers (CRUD)
- [ ] Create category handlers
- [ ] Create budget handlers
- [ ] Add error handling middleware

## Phase 7: Frontend Foundation
- [ ] Setup API client with proper types
- [ ] Create TransactionList component
- [ ] Display fake transactions
- [ ] Connect to backend API
- [ ] Add loading and error states

## Phase 8: Real-time Updates (SSE)
- [ ] Implement SSE endpoint on backend
- [ ] Create EventSource connection on frontend
- [ ] Handle auto-reconnection
- [ ] Update transaction list on SSE events
- [ ] Add connection status indicator

## Phase 9: Features Catchup with PureScript
- [ ] Transaction editing
- [ ] Transaction deletion
- [ ] Context menus
- [ ] Keyboard shortcuts
- [ ] Dark mode toggle
- [ ] Budget management UI
- [ ] Account management UI
- [ ] Category management UI

## Phase 10: Testing & Polish
- [ ] Unit tests for all repos
- [ ] Integration tests for handlers
- [ ] E2E tests with Playwright
- [ ] Performance optimization
- [ ] Error boundary implementation
- [ ] Logging system

## Phase 11: Internal Commands
- [ ] Create CLI interface for debugging
- [ ] Add commands for data manipulation
- [ ] Add commands for testing scenarios
- [ ] Document available commands

## Phase 12: Advanced Features
- [ ] Transaction search/filtering
- [ ] Bulk operations
- [ ] Import/Export functionality
- [ ] Transaction templates
- [ ] Recurring transactions
- [ ] Reports and analytics

## Notes
- Keep dependencies minimal
- Test-first approach for all features
- Use fake repos for rapid prototyping
- Ensure type safety throughout
- Follow FP principles (no unnecessary classes)
- Manual dependency injection via function parameters