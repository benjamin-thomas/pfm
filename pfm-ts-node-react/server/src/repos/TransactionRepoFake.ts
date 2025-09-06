// Fake implementation of TransactionRepo for testing
// Uses in-memory storage

import {
  Transaction,
  NewTransaction,
  UpdateTransaction,
  TransactionFilters,
  PaginationParams,
  PaginatedResponse,
} from '@shared/types';
import { TransactionRepo } from '@shared/repos';

export const createTransactionRepoFake = (): TransactionRepo => {
  // In-memory storage
  let transactions: Transaction[] = [];
  let nextId = 1;

  // Helper to create a transaction with auto-generated fields
  const createTransaction = (newTx: NewTransaction): Transaction => {
    const now = Math.floor(Date.now() / 1000);
    return {
      ...newTx,
      uniqueFitId: newTx.uniqueFitId || null,
      transactionId: nextId++,
      createdAt: now,
      updatedAt: now,
    };
  };

  // Helper to apply filters
  const applyFilters = (txs: Transaction[], filters?: TransactionFilters): Transaction[] => {
    if (!filters) return txs;

    return txs.filter((tx) => {
      if (filters.budgetId !== undefined && tx.budgetId !== filters.budgetId) return false;
      if (filters.fromAccountId !== undefined && tx.fromAccountId !== filters.fromAccountId) return false;
      if (filters.toAccountId !== undefined && tx.toAccountId !== filters.toAccountId) return false;
      if (filters.startDate !== undefined && tx.date < filters.startDate) return false;
      if (filters.endDate !== undefined && tx.date > filters.endDate) return false;
      if (filters.search) {
        const searchLower = filters.search.toLowerCase();
        if (!tx.descr.toLowerCase().includes(searchLower) &&
            !tx.descrOrig.toLowerCase().includes(searchLower)) {
          return false;
        }
      }
      return true;
    });
  };

  // Helper to apply pagination
  const paginate = <T>(items: T[], params?: PaginationParams): PaginatedResponse<T> => {
    const page = params?.page || 1;
    const pageSize = params?.pageSize || 50;
    const start = (page - 1) * pageSize;
    const end = start + pageSize;

    // Apply sorting
    if (params?.orderBy) {
      const key = params.orderBy as keyof T;
      const dir = params.orderDir || 'asc';
      items.sort((a, b) => {
        const aVal = a[key];
        const bVal = b[key];
        if (aVal < bVal) return dir === 'asc' ? -1 : 1;
        if (aVal > bVal) return dir === 'asc' ? 1 : -1;
        return 0;
      });
    }

    return {
      items: items.slice(start, end),
      total: items.length,
      page,
      pageSize,
    };
  };

  return {
    create: async (transaction: NewTransaction): Promise<Transaction> => {
      const newTx = createTransaction(transaction);
      transactions.push(newTx);
      return newTx;
    },

    findById: async (id: number): Promise<Transaction | null> => {
      return transactions.find((tx) => tx.transactionId === id) || null;
    },

    list: async (
      filters?: TransactionFilters,
      pagination?: PaginationParams
    ): Promise<PaginatedResponse<Transaction>> => {
      const filtered = applyFilters(transactions, filters);
      return paginate(filtered, pagination);
    },

    listByBudget: async (budgetId: number): Promise<Transaction[]> => {
      return transactions.filter((tx) => tx.budgetId === budgetId);
    },

    listByAccount: async (accountId: number): Promise<Transaction[]> => {
      return transactions.filter(
        (tx) => tx.fromAccountId === accountId || tx.toAccountId === accountId
      );
    },

    update: async (
      id: number,
      updates: UpdateTransaction
    ): Promise<Transaction | null> => {
      const index = transactions.findIndex((tx) => tx.transactionId === id);
      if (index === -1) return null;

      const existing = transactions[index]!;
      const updated: Transaction = {
        ...updates,
        transactionId: existing.transactionId,
        createdAt: existing.createdAt,
        updatedAt: Math.floor(Date.now() / 1000),
      };
      transactions[index] = updated;
      return updated;
    },

    delete: async (id: number): Promise<boolean> => {
      const index = transactions.findIndex((tx) => tx.transactionId === id);
      if (index === -1) return false;
      transactions.splice(index, 1);
      return true;
    },

    createMany: async (newTransactions: NewTransaction[]): Promise<Transaction[]> => {
      const created = newTransactions.map((tx) => createTransaction(tx));
      transactions.push(...created);
      return created;
    },

    deleteMany: async (ids: number[]): Promise<number> => {
      const idSet = new Set(ids);
      const before = transactions.length;
      transactions = transactions.filter((tx) => !idSet.has(tx.transactionId));
      return before - transactions.length;
    },
  };
};

// Export a function to create a fake repo with seed data
export const createTransactionRepoFakeWithSeedData = (): TransactionRepo => {
  const repo = createTransactionRepoFake();

  // Add seed data based on OFX fixture
  const seedData: NewTransaction[] = [
    {
      budgetId: 1,
      fromAccountId: 5, // Employer
      toAccountId: 2, // Checking account  
      date: new Date('2024-09-30').getTime() / 1000,
      descrOrig: 'Monthly Income',
      descr: 'Monthly Income',
      cents: 250000, // +2500.00 EUR
      uniqueFitId: 'TXN001',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 6, // Unknown_EXPENSE
      date: new Date('2024-09-25').getTime() / 1000,
      descrOrig: 'Rent Payment',
      descr: 'Rent Payment',
      cents: 80000, // -800.00 EUR
      uniqueFitId: 'TXN002',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 7, // Groceries
      date: new Date('2024-09-20').getTime() / 1000,
      descrOrig: 'Grocery Store',
      descr: 'Grocery Store',
      cents: 15000, // -150.00 EUR
      uniqueFitId: 'TXN003',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 9, // Transport
      date: new Date('2024-09-18').getTime() / 1000,
      descrOrig: 'Gas Station',
      descr: 'Gas Station',
      cents: 5000, // -50.00 EUR
      uniqueFitId: 'TXN004',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 13, // Leisure
      date: new Date('2024-09-15').getTime() / 1000,
      descrOrig: 'Coffee Shop',
      descr: 'Coffee Shop',
      cents: 2500, // -25.00 EUR
      uniqueFitId: 'TXN005',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 11, // Energy
      date: new Date('2024-09-10').getTime() / 1000,
      descrOrig: 'Utility Bill',
      descr: 'Utility Bill',
      cents: 10000, // -100.00 EUR
      uniqueFitId: 'TXN006',
    },
    {
      budgetId: 1,
      fromAccountId: 4, // Unknown_INCOME
      toAccountId: 2, // Checking account
      date: new Date('2024-09-05').getTime() / 1000,
      descrOrig: 'Other Income',
      descr: 'Other Income',
      cents: 20000, // +200.00 EUR
      uniqueFitId: 'TXN007',
    },
    {
      budgetId: 1,
      fromAccountId: 2, // Checking account
      toAccountId: 8, // Communications
      date: new Date('2024-09-01').getTime() / 1000,
      descrOrig: 'Phone Bill',
      descr: 'Phone Bill',
      cents: 3000, // -30.00 EUR
      uniqueFitId: 'TXN008',
    },
  ];

  // Block to add seed data synchronously (in real app, this would be awaited)
  Promise.all(seedData.map((tx) => repo.create(tx)));

  return repo;
};