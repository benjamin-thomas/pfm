import * as http from 'http';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { dispatchInit, isValidMethod, type HttpMethod } from './utils/dispatch';
import * as transactionHandlers from './handlers/transactionHandlers';
import * as balanceHandlers from './handlers/balanceHandlers';
import * as healthHandlers from './handlers/healthHandlers';
import * as sseHandlers from './handlers/sseHandlers';

interface Config {
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
const createServer = (transactionRepo: TransactionRepo): http.Server => {
  const config = getConfig();

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

      // Initialize dispatch and register routes for this request
      const dispatch = dispatchInit();
      dispatch.onMatch('GET', '/health', () => healthHandlers.check(req, res, config));
      dispatch.onMatchPstring('GET', '/hello/{name}', (name) => () => healthHandlers.hello(req, res, name));
      dispatch.onMatch('GET', '/api/transactions', () => transactionHandlers.getMany(req, res, transactionRepo));
      dispatch.onMatchPnumber('GET', '/api/transactions/{id}', (id) => () => transactionHandlers.getOne(req, res, transactionRepo, id));
      dispatch.onMatch('POST', '/api/transactions', () => transactionHandlers.create(req, res, transactionRepo));
      dispatch.onMatch('GET', '/api/balances', () => balanceHandlers.getAll(req, res));
      dispatch.onMatch('GET', '/api/events', () => sseHandlers.events(req, res, config));
      
      // Pure dispatch with just method and url
      const matched = await dispatch({
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
  const server = createServer(transactionRepo);

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

// Run the server
main();