import * as http from 'http';
import { URL } from 'url';
import type { TransactionRepo } from '../repos/transactionRepo';
import { NewTransactionSchema } from '../repos/transactionRepo';
import type { Transaction } from '@shared/types';

// Parse JSON body helper
const parseJsonBody = (req: http.IncomingMessage): Promise<unknown> => {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', (chunk) => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        resolve(body ? JSON.parse(body) : {});
      } catch (err) {
        reject(err);
      }
    });
    req.on('error', reject);
  });
};

// JSON response helper
const sendJson = (res: http.ServerResponse, statusCode: number, data: unknown): void => {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(data));
};

export const getMany = (repo: TransactionRepo) =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    const url = new URL(req.url || '', 'http://localhost');
    const budgetId = url.searchParams.get('budgetId');

    let transactions: Transaction[];
    if (budgetId) {
      transactions = await repo.listByBudget(parseInt(budgetId, 10));
    } else {
      const result = await repo.list();
      transactions = result.items;
    }

    sendJson(res, 200, transactions);
  };

export const getOne = (repo: TransactionRepo, id: number) =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    const transaction = await repo.findById(id);

    if (!transaction) {
      res.statusCode = 404;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: 'Transaction not found' }));
      return;
    }

    sendJson(res, 200, transaction);
  };

export const create = (repo: TransactionRepo) =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    const body = await parseJsonBody(req);

    // Validate request body with Zod
    const result = NewTransactionSchema.safeParse(body);
    if (!result.success) {
      sendJson(res, 400, {
        error: 'Invalid transaction data',
        issues: result.error.issues,
      });
      return;
    }

    const transaction = await repo.create(result.data);
    sendJson(res, 201, transaction);
  };