// Database entity types that match our SQL schema

export interface Category {
  categoryId: number;
  name: string;
  createdAt: number; // Unix timestamp
  updatedAt: number; // Unix timestamp
}

export interface Account {
  accountId: number;
  categoryId: number;
  name: string;
  createdAt: number;
  updatedAt: number;
}

export interface Transaction {
  transactionId: number;
  budgetId: number;
  fromAccountId: number;
  toAccountId: number;
  uniqueFitId: string | null;
  date: number; // Unix timestamp
  descrOrig: string;
  descr: string;
  cents: number; // Store as cents to avoid floating point issues
  createdAt: number;
  updatedAt: number;
}

export interface Budget {
  budgetId: number;
  startsOn: number; // Unix timestamp
  endsOn: number; // Unix timestamp
  createdAt: number;
  updatedAt: number;
}

export interface BudgetLine {
  budgetLineId: number;
  budgetId: number;
  accountId: number;
  cents: number;
  createdAt: number;
  updatedAt: number;
}

// DTOs for creating new entities (without auto-generated fields)
export type NewTransaction = Omit<Transaction, 'transactionId' | 'createdAt' | 'updatedAt'>;

export type NewAccount = Omit<Account, 'accountId' | 'createdAt' | 'updatedAt'>;

export type NewCategory = Omit<Category, 'categoryId' | 'createdAt' | 'updatedAt'>;

export type NewBudget = Omit<Budget, 'budgetId' | 'createdAt' | 'updatedAt'>;

export type NewBudgetLine = Omit<BudgetLine, 'budgetLineId' | 'createdAt' | 'updatedAt'>;

// DTOs for updating entities (full resource updates, not partial)
export type UpdateTransaction = Omit<Transaction, 'transactionId' | 'createdAt' | 'updatedAt'>;

export type UpdateAccount = Omit<Account, 'accountId' | 'createdAt' | 'updatedAt'>;

export type UpdateCategory = Omit<Category, 'categoryId' | 'createdAt' | 'updatedAt'>;

export type UpdateBudget = Omit<Budget, 'budgetId' | 'createdAt' | 'updatedAt'>;

export type UpdateBudgetLine = Omit<BudgetLine, 'budgetLineId' | 'createdAt' | 'updatedAt'>;

// View models for frontend display

export interface TransactionView extends Transaction {
  fromAccountName: string;
  toAccountName: string;
  fromCategoryName: string;
  toCategoryName: string;
  amountFormatted: string; // e.g., "$12.34"
}

export interface AccountView extends Account {
  categoryName: string;
  balance: number; // Current balance in cents
}

export interface AccountBalanceRead {
  accountId: number;
  accountName: string;
  categoryId: number;
  categoryName: string;
  accountBalance: number; // Balance in cents
}

// API response types

export interface PaginatedResponse<T> {
  items: T[];
  total: number;
  page: number;
  pageSize: number;
}

// Query parameters

export interface TransactionFilters {
  budgetId?: number;
  fromAccountId?: number;
  toAccountId?: number;
  startDate?: number;
  endDate?: number;
  search?: string;
}

export interface PaginationParams {
  page?: number;
  pageSize?: number;
  orderBy?: string;
  orderDir?: 'asc' | 'desc';
}


// Helper functions for money formatting

export const centsToDecimal = (cents: number): number => cents / 100;

export const decimalToCents = (decimal: number): number => Math.round(decimal * 100);

export const formatMoney = (cents: number): string => {
  const decimal = centsToDecimal(cents);
  return new Intl.NumberFormat('fr-FR', {
    style: 'currency', 
    currency: 'EUR',
  }).format(decimal);
};

// Helper functions for date handling

export const unixToDate = (timestamp: number): Date => new Date(timestamp * 1000);

export const dateToUnix = (date: Date): number => Math.floor(date.getTime() / 1000);

export const formatDate = (timestamp: number): string => {
  const date = unixToDate(timestamp);
  return date.toLocaleDateString('en-US', {
    year: 'numeric',
    month: 'short',
    day: 'numeric',
  });
};