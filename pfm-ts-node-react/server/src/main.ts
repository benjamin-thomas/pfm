import * as http from 'http';
import { z } from 'zod';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { httpDispatch2Init } from './utils/httpDispatch2';
import * as transactionHandlers from './handlers/transactionHandlers';
import * as balanceHandlers from './handlers/balanceHandlers';
import * as healthHandlers from './handlers/healthHandlers';
import * as sseHandlers from './handlers/sseHandlers';
import { clientManagerInit } from './services/clientManager';

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

// ======================================================================
// HANDLER CREATES CONTEXT PATTERN
// ======================================================================
// This is a functional programming pattern where:
// 1. Handlers don't receive context - they CREATE the execution context
// 2. The execution context is a function that will process the HTTP request
// 3. The router calls handler() to get the execution function, then runs it
//
// Benefits:
// - No forced async - handlers decide if they're sync/async via the execution context
// - Clean dependency injection - dependencies bound at handler creation time
// - Simple function composition
// - No context object impedance mismatch - execution context IS the request processor
// - Test simplicity - test handlers return sync NOOPs, production returns mostly async processors
//
// Flow: handler(deps) -> ()                  -> (req, res) -> Promise<void> | void
//       ^dependency      ^execution context     ^actual HTTP processing
//
type RequestContext = (req: http.IncomingMessage, res: http.ServerResponse) => Promise<void> | void;


// Create HTTP server with dependency injection
export const serverInit = (transactionRepo: TransactionRepo, config: Config): http.Server => {
  // Create the client manager for SSE connections
  const clientManager = clientManagerInit();

  // Build the trie, once at startup
  // Error callbacks return execution contexts (following the same pattern as handlers)
  // Instead of performing side effects directly, they return functions that will perform them
  const httpDispatch = httpDispatch2Init<RequestContext>({
    onMatchBadParam: (msg) => (_req, res) => sendJson(res, 400, { error: msg }),
    onMatchNotFound: (msg) => (_req, res) => sendJson(res, 404, { error: msg })
  });

  {
    httpDispatch.matchP0('GET', '/health', healthHandlers.check(config));

    httpDispatch.matchP0('GET', '/api/events', sseHandlers.events(config, clientManager));
    httpDispatch.matchP0('GET', '/api/status', sseHandlers.status(clientManager));

    httpDispatch.matchP1(
      'GET', '/hello/?', z.string(),
      (name) => healthHandlers.hello(name),
    );

    // Transactions
    {
      httpDispatch.matchP0('GET', '/api/transactions', transactionHandlers.getMany(transactionRepo));
      httpDispatch.matchP0('POST', '/api/transactions', transactionHandlers.create(transactionRepo));
      httpDispatch.matchP1(
        'GET', '/api/transactions/?',
        z.coerce.number(),
        (id) => transactionHandlers.getOne(transactionRepo, id),
      );
    }

    httpDispatch.matchP0('GET', '/api/balances', balanceHandlers.getAll);
  }

  // Main request handler
  const server = http.createServer(async (req: http.IncomingMessage, res) => {
    if (!req.method) return sendJson(res, 400, "Bad client, go away!");
    if (!req.url) return sendJson(res, 400, "Bad client, go away!");

    const queryIndex = req.url.indexOf('?');
    const reqPath: string = queryIndex === -1 ? req.url : req.url.substring(0, queryIndex);
    // console.log({ reqPath });

    // console.log({ reqUrl: req.url, reqMethod: req.method, reqPath: new URL(req.url, req.headers.host).pathname });

    // Set CORS headers for all requests
    setCorsHeaders(res, config.frontendUrl);

    // Handle preflight requests
    if (req.method === 'OPTIONS') {
      res.statusCode = 204;
      res.end();
      return;
    }

    try {

      // ======================================================================
      // THE MAGIC HAPPENS HERE - HANDLER CREATES CONTEXT PATTERN IN ACTION
      // ======================================================================
      // 1. Router finds the handler for this method/path
      // 2. Handler returns an execution context (a function)
      // 3. We provide an executor that calls executionContext(req, res)
      // 4. The execution context processes the HTTP request
      //
      // This is the key insight: the handler creates the execution context,
      // and we just provide the parameters (req, res) to run it
      await httpDispatch.run(req.method, reqPath, (executionContext) => executionContext(req, res));
    } catch (error) {
      console.error('Unexpected handler error:', error);
      sendJson(res, 500, { error: 'Internal server error' });
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
  const server = serverInit(transactionRepo, config);

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

// Run the server only if this file is executed directly (not imported/tested)
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}