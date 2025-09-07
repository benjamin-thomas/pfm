// Transaction Repository interface
// Both fake and SQL implementations must satisfy this interface

import { z } from 'zod';
import type {
  Transaction,
  NewTransaction,
  UpdateTransaction,
  TransactionFilters,
  PaginationParams,
  PaginatedResponse,
} from '@shared/types';

// Input validation schemas for server use
export const NewTransactionSchema = z.object({
  budgetId: z.number(),
  fromAccountId: z.number(),
  toAccountId: z.number(),
  uniqueFitId: z.string().nullable(),
  date: z.number(), // Unix timestamp
  descrOrig: z.string(),
  descr: z.string(),
  cents: z.number(), // Cents to avoid floating point issues
});

export const UpdateTransactionSchema = NewTransactionSchema;

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