// Shared Zod schemas for API responses
// These can be used by both server and client for type-safe communication

import { z } from 'zod';

// Core entity response schemas
export const TransactionSchema = z.object({
  transactionId: z.number(),
  budgetId: z.number(),
  fromAccountId: z.number(),
  toAccountId: z.number(),
  uniqueFitId: z.string().nullable(),
  date: z.number(), // Unix timestamp
  descrOrig: z.string(),
  descr: z.string(),
  cents: z.number(),
  createdAt: z.number(),
  updatedAt: z.number(),
});

export const AccountBalanceReadSchema = z.object({
  accountId: z.number(),
  accountName: z.string(),
  categoryId: z.number(),
  categoryName: z.string(),
  accountBalance: z.number(), // Balance in cents
});

// Generic paginated response schema
export const createPaginatedResponseSchema = <T extends z.ZodTypeAny>(itemSchema: T) =>
  z.object({
    items: z.array(itemSchema),
    total: z.number(),
    page: z.number(),
    pageSize: z.number(),
  });

export const PaginatedTransactionResponseSchema = createPaginatedResponseSchema(TransactionSchema);

// API endpoint response schemas
export const HealthResponseSchema = z.object({
  status: z.literal('ok'),
  env: z.string(),
  port: z.number(),
});

export const ErrorResponseSchema = z.object({
  error: z.string(),
  issues: z.array(z.any()).optional(), // Zod validation issues
});

// SSE event schema
export const SSEEventSchema = z.object({
  type: z.string(),
  timestamp: z.number(),
});

// Infer TypeScript types from schemas
export type Transaction = z.infer<typeof TransactionSchema>;
export type AccountBalanceRead = z.infer<typeof AccountBalanceReadSchema>;
export type PaginatedTransactionResponse = z.infer<typeof PaginatedTransactionResponseSchema>;
export type HealthResponse = z.infer<typeof HealthResponseSchema>;
export type ErrorResponse = z.infer<typeof ErrorResponseSchema>;
export type SSEEvent = z.infer<typeof SSEEventSchema>;