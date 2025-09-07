import * as http from 'http';
import type { AccountBalanceRead } from '@shared/types';

// JSON response helper
const sendJson = (res: http.ServerResponse, statusCode: number, data: unknown): void => {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(data));
};

export const getAll = () =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    const balances: AccountBalanceRead[] = [
      {
        accountId: 2,
        accountName: 'Checking account',
        categoryId: 1,
        categoryName: 'Assets',
        accountBalance: 154500, // +1545.00 EUR in cents
      },
      {
        accountId: 3,
        accountName: 'Savings account',
        categoryId: 1,
        categoryName: 'Assets',
        accountBalance: 500000, // +5000.00 EUR in cents
      },
      {
        accountId: 6,
        accountName: 'Unknown_EXPENSE',
        categoryId: 2,
        categoryName: 'Expenses',
        accountBalance: -102500, // -1025.00 EUR in cents
      }
    ];
    sendJson(res, 200, balances);
  };