// Repository interfaces - define the contract for data access
// Both fake and SQL implementations must satisfy these interfaces

import type {
  Transaction,
  NewTransaction,
  UpdateTransaction,
  Account,
  NewAccount,
  UpdateAccount,
  Category,
  NewCategory,
  UpdateCategory,
  Budget,
  NewBudget,
  UpdateBudget,
  BudgetLine,
  NewBudgetLine,
  UpdateBudgetLine,
  TransactionFilters,
  PaginationParams,
  PaginatedResponse,
} from './types';

// Transaction Repository
export interface TransactionRepo {
  // Create
  create(transaction: NewTransaction): Promise<Transaction>;
  
  // Read
  findById(id: number): Promise<Transaction | null>;
  list(filters?: TransactionFilters, pagination?: PaginationParams): Promise<PaginatedResponse<Transaction>>;
  listByBudget(budgetId: number): Promise<Transaction[]>;
  listByAccount(accountId: number): Promise<Transaction[]>;
  
  // Update (full resource replacement)
  update(id: number, updates: UpdateTransaction): Promise<Transaction | null>;
  
  // Delete
  delete(id: number): Promise<boolean>;
  
  // Bulk operations
  createMany(transactions: NewTransaction[]): Promise<Transaction[]>;
  deleteMany(ids: number[]): Promise<number>; // Returns count of deleted
}

// Account Repository
export interface AccountRepo {
  // Create
  create(account: NewAccount): Promise<Account>;
  
  // Read
  findById(id: number): Promise<Account | null>;
  findByName(name: string): Promise<Account | null>;
  list(): Promise<Account[]>;
  listByCategory(categoryId: number): Promise<Account[]>;
  
  // Update (full resource replacement)
  update(id: number, updates: UpdateAccount): Promise<Account | null>;
  
  // Delete
  delete(id: number): Promise<boolean>;
  
  // Special queries
  getBalance(accountId: number): Promise<number>; // Returns balance in cents
}

// Category Repository
export interface CategoryRepo {
  // Create
  create(category: NewCategory): Promise<Category>;
  
  // Read
  findById(id: number): Promise<Category | null>;
  findByName(name: string): Promise<Category | null>;
  list(): Promise<Category[]>;
  
  // Update (full resource replacement)
  update(id: number, updates: UpdateCategory): Promise<Category | null>;
  
  // Delete
  delete(id: number): Promise<boolean>;
}

// Budget Repository
export interface BudgetRepo {
  // Create
  create(budget: NewBudget): Promise<Budget>;
  
  // Read
  findById(id: number): Promise<Budget | null>;
  list(): Promise<Budget[]>;
  findByDate(date: number): Promise<Budget | null>; // Find budget containing this date
  getCurrentBudget(): Promise<Budget | null>;
  
  // Update (full resource replacement)
  update(id: number, updates: UpdateBudget): Promise<Budget | null>;
  
  // Delete
  delete(id: number): Promise<boolean>;
  
  // Check for overlaps
  hasOverlap(startsOn: number, endsOn: number, excludeId?: number): Promise<boolean>;
}

// Budget Line Repository
export interface BudgetLineRepo {
  // Create
  create(budgetLine: NewBudgetLine): Promise<BudgetLine>;
  
  // Read
  findById(id: number): Promise<BudgetLine | null>;
  listByBudget(budgetId: number): Promise<BudgetLine[]>;
  listByAccount(accountId: number): Promise<BudgetLine[]>;
  
  // Update (full resource replacement)
  update(id: number, updates: UpdateBudgetLine): Promise<BudgetLine | null>;
  
  // Delete
  delete(id: number): Promise<boolean>;
  deleteByBudget(budgetId: number): Promise<number>; // Returns count
  
  // Bulk operations
  createMany(budgetLines: NewBudgetLine[]): Promise<BudgetLine[]>;
  replaceBudgetLines(budgetId: number, newLines: NewBudgetLine[]): Promise<BudgetLine[]>;
}

// Unit of Work pattern for transactions (database transactions, not financial)
export interface UnitOfWork {
  transactionRepo: TransactionRepo;
  accountRepo: AccountRepo;
  categoryRepo: CategoryRepo;
  budgetRepo: BudgetRepo;
  budgetLineRepo: BudgetLineRepo;
  
  // Commit all changes or rollback on error
  commit(): Promise<void>;
  rollback(): Promise<void>;
}

// Factory for creating repository instances
export interface RepoFactory {
  createTransactionRepo(): TransactionRepo;
  createAccountRepo(): AccountRepo;
  createCategoryRepo(): CategoryRepo;
  createBudgetRepo(): BudgetRepo;
  createBudgetLineRepo(): BudgetLineRepo;
  createUnitOfWork(): UnitOfWork;
}