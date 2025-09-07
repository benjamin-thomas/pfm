import * as http from 'http';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { createRouter } from './utils/router';
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

  // Register all routes declaratively
  const router = createRouter();
  router.onMatch('GET', '/health', healthHandlers.check(config));
  router.onMatchPstring('GET', '/hello/{name}', healthHandlers.hello);
  router.onMatch('GET', '/api/transactions', transactionHandlers.getMany(transactionRepo));
  router.onMatchPnumber('GET', '/api/transactions/{id}', transactionHandlers.getOne(transactionRepo));
  router.onMatch('POST', '/api/transactions', transactionHandlers.create(transactionRepo));
  router.onMatch('GET', '/api/balances', balanceHandlers.getAll());
  router.onMatch('GET', '/api/events', sseHandlers.events(config));

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
      const routed = await router.run(req, res);
      if (!routed) {
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