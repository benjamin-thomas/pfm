import { describe, it, expect, beforeAll, afterAll } from 'vitest';
import * as http from 'http';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { createServer, type Config } from './main';

// Simple HTTP request helper for testing
const makeRequest = (port: number, path: string, method = 'GET'): Promise<{ statusCode: number; data: string }> => {
  return new Promise((resolve, reject) => {
    const req = http.request({
      hostname: 'localhost',
      port,
      path,
      method,
    }, (res) => {
      let data = '';
      res.on('data', (chunk) => {
        data += chunk;
      });
      res.on('end', () => {
        resolve({
          statusCode: res.statusCode || 0,
          data,
        });
      });
    });

    req.on('error', (err) => {
      reject(err);
    });

    req.end();
  });
};

describe('Server E2E Tests', () => {
  let server: http.Server;
  let transactionRepo: TransactionRepo;
  let actualPort: number; // Will be set to the randomly assigned port
  const testConfig: Config = {
    env: 'test',
    port: 0, // 0 means "assign any available port"
    frontendUrl: 'http://localhost:4005', // Test frontend URL
  };

  beforeAll(async () => {
    // Create fake repository with seed data
    transactionRepo = createTransactionRepoFakeWithSeedData();
    
    // Create server with test config
    server = createServer(transactionRepo, testConfig);
    
    // Start the server with port 0 (random port assignment)
    await new Promise<void>((resolve, reject) => {
      server.listen(testConfig.port, (err?: Error) => {
        if (err) reject(err);
        else {
          // Extract the actual assigned port
          const address = server.address();
          if (address && typeof address === 'object') {
            actualPort = address.port;
          } else {
            reject(new Error('Could not get server port'));
            return;
          }
          resolve();
        }
      });
    });
    
    // Wait a bit for server to be ready
    await new Promise(resolve => setTimeout(resolve, 100));
  });

  afterAll(async () => {
    if (server) {
      await new Promise<void>((resolve) => {
        server.close(() => resolve());
      });
    }
  });

  describe('Health endpoint', () => {
    it('should return health status', async () => {
      const response = await makeRequest(actualPort, '/health');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(data).toEqual({
        status: 'ok',
        env: 'test',
        port: 0, // Config port, not the actual assigned port
      });
    });
  });

  describe('Hello endpoint', () => {
    it('should return personalized greeting', async () => {
      const response = await makeRequest(actualPort, '/hello/alice');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(data).toEqual({
        message: 'Hello, alice!',
      });
    });

    it('should handle special characters in name', async () => {
      const response = await makeRequest(actualPort, '/hello/test-user');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(data).toEqual({
        message: 'Hello, test-user!',
      });
    });
  });

  describe('Transactions endpoints', () => {
    it('should return list of transactions', async () => {
      const response = await makeRequest(actualPort, '/api/transactions');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(Array.isArray(data)).toBe(true);
      expect(data.length).toBeGreaterThan(0);
      
      // Check structure of first transaction
      const firstTransaction = data[0];
      expect(firstTransaction).toHaveProperty('transactionId');
      expect(firstTransaction).toHaveProperty('budgetId');
      expect(firstTransaction).toHaveProperty('fromAccountId');
      expect(firstTransaction).toHaveProperty('toAccountId');
      expect(firstTransaction).toHaveProperty('descr');
      expect(firstTransaction).toHaveProperty('cents');
    });

    it('should return specific transaction by id', async () => {
      const response = await makeRequest(actualPort, '/api/transactions/1');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(data).toHaveProperty('transactionId', 1);
      expect(data).toHaveProperty('descr');
    });

    it('should return 404 for non-existent transaction', async () => {
      const response = await makeRequest(actualPort, '/api/transactions/999');
      
      expect(response.statusCode).toBe(404);
      
      const data = JSON.parse(response.data);
      expect(data).toHaveProperty('error', 'Transaction not found');
    });

    it('should handle invalid transaction id', async () => {
      const response = await makeRequest(actualPort, '/api/transactions/invalid');
      
      expect(response.statusCode).toBe(500);
      
      const data = JSON.parse(response.data);
      // Zod error gets stringified by the error handler
      expect(data).toHaveProperty('error');
      expect(data.error).toContain('invalid_type');
      expect(data.error).toContain('expected');
      expect(data.error).toContain('number');
    });
  });

  describe('Balances endpoint', () => {
    it('should return list of account balances', async () => {
      const response = await makeRequest(actualPort, '/api/balances');
      
      expect(response.statusCode).toBe(200);
      
      const data = JSON.parse(response.data);
      expect(Array.isArray(data)).toBe(true);
      expect(data.length).toBeGreaterThan(0);
      
      // Check structure of first balance
      const firstBalance = data[0];
      expect(firstBalance).toHaveProperty('accountId');
      expect(firstBalance).toHaveProperty('accountName');
      expect(firstBalance).toHaveProperty('categoryId');
      expect(firstBalance).toHaveProperty('categoryName');
      expect(firstBalance).toHaveProperty('accountBalance');
    });
  });

  describe('404 handling', () => {
    it('should return 404 for unknown endpoints', async () => {
      const response = await makeRequest(actualPort, '/unknown/endpoint');
      
      expect(response.statusCode).toBe(404);
      
      const data = JSON.parse(response.data);
      expect(data).toHaveProperty('error', 'Not found');
    });
  });
});

