import * as http from 'http';
import { z } from 'zod';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { httpDispatchInit, isValidMethod } from './utils/httpDispatch';
import * as transactionHandlers from './handlers/transactionHandlers';
import * as balanceHandlers from './handlers/balanceHandlers';
import * as healthHandlers from './handlers/healthHandlers';
import * as sseHandlers from './handlers/sseHandlers';

export interface Config {
  env: string;
  port: number;
  frontendUrl: string;
}

// Configuration based on environment
const getConfig = (): Config => {
  const env = process.env.APP_ENV || 'dev';
  const isDev = env === 'dev';

  return {
    env,
    port: isDev ? 8083 : 8084,
    frontendUrl: isDev ? 'http://localhost:4003' : 'http://localhost:4004',
  };
};

// CORS headers helper
const setCorsHeaders = (res: http.ServerResponse, frontendUrl: string): void => {
  res.setHeader('Access-Control-Allow-Origin', frontendUrl);
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  res.setHeader('Access-Control-Max-Age', '86400');
};

// JSON response helper
const sendJson = (res: http.ServerResponse, statusCode: number, data: unknown): void => {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(data));
};

// Create HTTP server with dependency injection
export const createServer = (transactionRepo: TransactionRepo, config: Config): http.Server => {

  // We'll register routes inside the request handler where we have req/res

  // Main request handler
  const server = http.createServer(async (req, res) => {
    // Set CORS headers for all requests
    setCorsHeaders(res, config.frontendUrl);

    // Handle preflight requests
    if (req.method === 'OPTIONS') {
      res.statusCode = 204;
      res.end();
      return;
    }

    try {
      // Strict validation - throw if missing or invalid
      if (!req.url) {
        throw new Error('URL is required');
      }
      if (!isValidMethod(req.method)) {
        throw new Error(`Invalid method: ${req.method}`);
      }

      // Initialize httpDispatch and register routes for this request
      const httpDispatch = httpDispatchInit();
      httpDispatch.onMatchAsync('GET', '/health', () => healthHandlers.check(req, res, config));
      httpDispatch.onMatchP_Async('GET', '/hello/{name}', z.string(), (name) => healthHandlers.hello(req, res, name));
      httpDispatch.onMatchAsync('GET', '/api/transactions', () => transactionHandlers.getMany(req, res, transactionRepo));
      httpDispatch.onMatchP_Async(
        'GET', '/api/transactions/{id}',
        z.coerce.number(),
        (id) => transactionHandlers.getOne(req, res, transactionRepo, id)
      );
      httpDispatch.onMatchAsync('POST', '/api/transactions', () => transactionHandlers.create(req, res, transactionRepo));
      httpDispatch.onMatchAsync('GET', '/api/balances', () => balanceHandlers.getAll(req, res));
      httpDispatch.onMatchSync('GET', '/api/events', () => sseHandlers.events(req, res, config));

      // Pure dispatch with just method and url
      const matched = await httpDispatch.run({
        method: req.method,
        url: req.url
      });

      if (!matched) {
        sendJson(res, 404, { error: 'Not found' });
      }
    } catch (error) {
      console.error('Route handler error:', error);
      const statusCode = error instanceof Error && error.name === 'ValidationError' ? 400 : 500;
      const message = error instanceof Error ? error.message : 'Internal server error';
      sendJson(res, statusCode, { error: message });
    }
  });

  return server;
};

// Main entry point
const main = (): void => {
  const config = getConfig();

  // Create repository (fake for now, will be SQL later)
  const transactionRepo = createTransactionRepoFakeWithSeedData();

  // Create and start server
  const server = createServer(transactionRepo, config);

  server.listen(config.port, () => {
    console.log(`🚀 Server running in ${config.env} mode on http://localhost:${config.port}`);
    console.log(`📡 Expecting frontend at ${config.frontendUrl}`);
  });

  // Graceful shutdown
  process.on('SIGTERM', () => {
    console.log('SIGTERM received, shutting down gracefully...');
    server.close(() => {
      console.log('Server closed');
      process.exit(0);
    });
  });
};

// Run the server only if this file is executed directly (not imported)
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}