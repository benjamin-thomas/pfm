import http from 'http';
import { URL } from 'url';
import { createTransactionRepoFakeWithSeedData } from '@src/repos/TransactionRepoFake';
import { TransactionRepo } from '@shared/repos';
import { Transaction, AccountBalanceRead } from '@shared/types';

// Configuration based on environment
const getConfig = () => {
  const env = process.env.APP_ENV || 'dev';
  const isDev = env === 'dev';

  return {
    env,
    port: isDev ? 8083 : 8084,
    frontendUrl: isDev ? 'http://localhost:4003' : 'http://localhost:4004',
  };
};

// CORS headers helper
const setCorsHeaders = (res: http.ServerResponse, frontendUrl: string) => {
  res.setHeader('Access-Control-Allow-Origin', frontendUrl);
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
  res.setHeader('Access-Control-Max-Age', '86400');
};

// JSON response helper
const sendJson = (res: http.ServerResponse, statusCode: number, data: any) => {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(data));
};

// Parse JSON body helper
const parseJsonBody = (req: http.IncomingMessage): Promise<any> => {
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

// Route matcher helper
type RouteHandler = (
  req: http.IncomingMessage,
  res: http.ServerResponse,
  params: Record<string, string>
) => Promise<void> | void;

interface Route {
  method: string;
  pattern: RegExp;
  handler: RouteHandler;
}

const createRouter = () => {
  const routes: Route[] = [];

  const add = (method: string, pattern: string, handler: RouteHandler) => {
    // Convert path pattern to regex, e.g., "/transactions/:id" -> /^\/transactions\/([^\/]+)$/
    const regexPattern = pattern
      .replace(/:[^\/]+/g, '([^/]+)')
      .replace(/\//g, '\\/');
    routes.push({
      method,
      pattern: new RegExp(`^${regexPattern}$`),
      handler,
    });
  };

  const match = (method: string, path: string): { handler: RouteHandler; params: Record<string, string> } | null => {
    for (const route of routes) {
      if (route.method !== method) continue;
      const match = path.match(route.pattern);
      if (match) {
        // Extract params from the matched groups
        const params: Record<string, string> = {};
        const paramNames = (route.pattern.source.match(/\([^)]+\)/g) || []).map((_, i) => `param${i}`);
        match.slice(1).forEach((value, i) => {
          params[paramNames[i] || `param${i}`] = value;
        });
        return { handler: route.handler, params };
      }
    }
    return null;
  };

  return { add, match };
};

// Create HTTP server with dependency injection
const createServer = (transactionRepo: TransactionRepo) => {
  const config = getConfig();
  const router = createRouter();

  // Health check
  router.add('GET', '/health', async (_req, res) => {
    sendJson(res, 200, { status: 'ok', env: config.env, port: config.port });
  });

  // List transactions
  router.add('GET', '/api/transactions', async (req, res) => {
    const url = new URL(req.url || '', `http://localhost:${config.port}`);
    const budgetId = url.searchParams.get('budgetId');

    let transactions: Transaction[];
    if (budgetId) {
      transactions = await transactionRepo.listByBudget(parseInt(budgetId, 10));
    } else {
      const result = await transactionRepo.list();
      transactions = result.items;
    }

    sendJson(res, 200, transactions);
  });

  // Get single transaction
  router.add('GET', '/api/transactions/:id', async (_req, res, params) => {
    const id = parseInt(params.param0 || '0', 10);
    const transaction = await transactionRepo.findById(id);

    if (!transaction) {
      res.statusCode = 404;
      res.setHeader('Content-Type', 'application/json');
      res.end(JSON.stringify({ error: 'Transaction not found' }));
      return;
    }

    sendJson(res, 200, transaction);
  });

  // Create transaction
  router.add('POST', '/api/transactions', async (req, res) => {
    const body = await parseJsonBody(req);
    const transaction = await transactionRepo.create(body);
    sendJson(res, 201, transaction);
  });

  // Get balance cards data (fake data matching original PureScript structure)
  router.add('GET', '/api/balances', async (_req, res) => {
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
  });

  // SSE endpoint for real-time updates
  router.add('GET', '/api/events', (req, res) => {
    res.writeHead(200, {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive',
      'Access-Control-Allow-Origin': config.frontendUrl,
    });

    // Send initial connection message
    res.write(`data: ${JSON.stringify({ type: 'connected', timestamp: Date.now() })}\n\n`);

    // Keep connection alive with heartbeat
    const heartbeat = setInterval(() => {
      res.write(`:heartbeat\n\n`);
    }, 30000);

    // Clean up on client disconnect
    req.on('close', () => {
      clearInterval(heartbeat);
    });
  });

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

    // Parse URL and find matching route
    const url = new URL(req.url || '', `http://localhost:${config.port}`);
    const routeMatch = router.match(req.method || 'GET', url.pathname);

    if (routeMatch) {
      try {
        await routeMatch.handler(req, res, routeMatch.params);
      } catch (error) {
        console.error('Route handler error:', error);
        const statusCode = error instanceof Error && error.name === 'ValidationError' ? 400 : 500;
        const message = error instanceof Error ? error.message : 'Internal server error';
        sendJson(res, statusCode, { error: message });
      }
    } else {
      sendJson(res, 404, { error: 'Not found' });
    }
  });

  return server;
};

// Main entry point
const main = () => {
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