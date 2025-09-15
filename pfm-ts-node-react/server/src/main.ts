import * as http from 'http';
import { z } from 'zod';
import { createTransactionRepoFakeWithSeedData } from './repos/TransactionRepoFake';
import type { TransactionRepo } from './repos/transactionRepo';
import { httpDispatch2Init } from './utils/httpDispatch2';
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

// Define context type for requests
type RequestContext = {
  req: http.IncomingMessage;
  res: http.ServerResponse;
};


// Create HTTP server with dependency injection
export const serverInit = (transactionRepo: TransactionRepo, config: Config): http.Server => {

  // Build the trie, once at startup
  const httpDispatch = httpDispatch2Init<RequestContext>({
    onMatchBadParam: ({ res }, msg) => sendJson(res, 400, { error: msg }),
    onMatchNotFound: ({ res }) => sendJson(res, 404, { error: 'Not found' })
  });

  {
    httpDispatch.matchP0('GET', '/health', async (ctx) => healthHandlers.check(ctx.req, ctx.res, config));

    httpDispatch.matchP1(
      'GET', '/hello/?', z.string(),
      async (ctx, name) => healthHandlers.hello(ctx.req, ctx.res, name),
    );

    // TODO: use currying to reduce verbosity here (the (req, res) passing)
    httpDispatch.matchP0('GET', '/api/transactions', async ({ req, res }) => transactionHandlers.getMany(req, res, transactionRepo));

    httpDispatch.matchP1(
      'GET', '/api/transactions/?',
      z.coerce.number(),
      async (ctx, id) => transactionHandlers.getOne(ctx.req, ctx.res, transactionRepo, id)
    );

    httpDispatch.matchP0('POST', '/api/transactions', async (ctx) => transactionHandlers.create(ctx.req, ctx.res, transactionRepo));

    httpDispatch.matchP0('GET', '/api/balances', async (ctx) => balanceHandlers.getAll(ctx.req, ctx.res));

    httpDispatch.matchP0('GET', '/api/events', async (ctx) => sseHandlers.events(ctx.req, ctx.res, config));
  }

  // Main request handler
  const server = http.createServer(async (req: http.IncomingMessage, res) => {
    if (!req.method) return sendJson(res, 400, "Bad client, go away!");
    if (!req.url) return sendJson(res, 400, "Bad client, go away!");

    const queryIndex = req.url.indexOf('?');
    const reqPath: string = queryIndex === -1 ? req.url : req.url.substring(0, queryIndex);
    console.log({ reqPath });

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

      // Pure dispatch with context - errors handled by callbacks
      await httpDispatch.run({ req, res }, req.method, reqPath);
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

// Run the server only if this file is executed directly (not imported)
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}