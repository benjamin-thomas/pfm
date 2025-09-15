#!/usr/bin/env npx tsx

// HTTP Dispatch v2 REPL - Interactive testing
// Usage: 
//   npx tsx httpDispatch2.repl.ts
//   REPL=1 npx tsx httpDispatch2.repl.ts

/*

echo -e "demo\nGET /health\nGET /greet/Alice\nGET /double/21\nGET /add/15/27\nGET /events/2024-12-25\nGET /resource/550e8400-e29b-41d4-a716-446655440000\nGET /multiply/6/7\nGET /between/2024-01-01/2024-12-31\nGET /nonexistent\nPOST /health\nquit" | npx tsx src/utils/httpDispatch2.repl.ts

TEST=1 npx tsx src/utils/httpDispatch2.repl.ts

*/

import { z } from 'zod';
import * as readline from 'readline';
import { httpDispatch2Init, type HttpDispatcher } from './httpDispatch2';

// Custom types for demo
type UUID = { uuid: string };
type ReplContext = () => void; // "empty" execution context, a NOOP (but could have been a Promise)

const setupDemoRoutes = (dispatcher: HttpDispatcher<ReplContext>): void => {
  // P0 routes - return execution contexts (NOOP functions that log)
  dispatcher.matchP0('GET', '/health', () => {
    console.log('Health check: OK');
  });

  dispatcher.matchP0('GET', '/api/info', () => {
    console.log('API version 2.0 (Trie-based)');
  });

  // P1 routes with basic types
  dispatcher.matchP1('GET', '/greet/?', z.string(), (name) => () => {
    console.log(`Hello, ${name}!`);
  });

  dispatcher.matchP1('GET', '/double/?', z.coerce.number(), (n) => () => {
    console.log(`${n} * 2 = ${n * 2}`);
  });

  // P1 routes with custom types
  const uuidSchema = z.string().regex(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i)
    .transform(s => ({ uuid: s }));

  dispatcher.matchP1('GET', '/resource/?', uuidSchema, (resource: UUID) => () => {
    console.log(`Fetching resource: ${resource.uuid}`);
  });

  const dateSchema = z.string().regex(/^\d{4}-\d{2}-\d{2}$/);

  dispatcher.matchP1('GET', '/events/?', dateSchema, (date) => () => {
    console.log(`Events for date: ${date}`);
  });

  // P2 routes
  dispatcher.matchP2('GET', '/add/?/?', z.coerce.number(), z.coerce.number(), (a, b) => () => {
    console.log(`${a} + ${b} = ${a + b}`);
  });

  dispatcher.matchP2('GET', '/between/?/?', dateSchema, dateSchema, (from, to) => () => {
    console.log(`Date range: ${from} to ${to}`);
  });

  dispatcher.matchP2('GET', '/multiply/?/?', z.coerce.number(), z.coerce.number(), (a, b) => () => {
    console.log(`${a} × ${b} = ${a * b}`);
  });

  console.log('Demo routes loaded!');
};

const runRepl = async (): Promise<void> => {
  const dispatcher = httpDispatch2Init<ReplContext>({
    onMatchBadParam: (msg) => () => console.log(`Error: ${msg}`),
    onMatchNotFound: (msg) => () => console.log(`Not Found: ${msg}`)
  });

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  console.log('\n=== HTTP Route Dispatcher REPL ===');
  console.log('Commands:');
  console.log('  <METHOD> <PATH>  - Dispatch a route (e.g., "GET /health")');
  console.log('  demo             - Load demo routes');
  console.log('  quit             - Exit\n');

  const askQuestion = (prompt: string): Promise<string> => {
    return new Promise((resolve) => {
      rl.question(prompt, (answer) => {
        resolve(answer.trim());
      });
    });
  };

  while (true) {
    const input = await askQuestion('> ');

    if (!input || input === 'quit' || input === 'exit') {
      console.log('Goodbye!');
      rl.close();
      break;
    }

    if (input === '') {
      continue;
    }

    if (input === 'demo') {
      setupDemoRoutes(dispatcher);
      continue;
    }

    const parts = input.split(' ');
    const [method, path] = parts;
    if (method && path) {
      dispatcher.run(method, path, (execCtx) => execCtx());
      console.log('');
    } else {
      console.log('Usage: <METHOD> <PATH>');
    }
  }
  process.exit(0);
};

// Automated test mode
const runAutomatedTest = async (): Promise<void> => {
  const dispatcher = httpDispatch2Init<ReplContext>({
    onMatchBadParam: (msg) => () => console.log(`Error: ${msg}`),
    onMatchNotFound: (msg) => () => console.log(`Not Found: ${msg}`)
  });

  console.log('\n=== Running Automated Test ===\n');

  setupDemoRoutes(dispatcher);

  const testInputs = [
    'GET /health',
    'GET /greet/Alice',
    'GET /double/21',
    'GET /add/15/27',
    'GET /events/2024-12-25',
    'GET /resource/550e8400-e29b-41d4-a716-446655440000',
    'GET /multiply/6/7',
    'GET /between/2024-01-01/2024-12-31',
    'GET /nonexistent',
    'POST /health'
  ];

  console.log('Test Output:');
  console.log('============');

  for (const input of testInputs) {
    console.log(`> ${input}`);
    const [method, path] = input.split(' ');
    if (method && path) {
      dispatcher.run(method, path, (execCtx) => execCtx());
    }
    console.log('');
  }

  process.exit(0);
};

// Main entry point
if (process.env.REPL === '1') {
  runRepl().catch(console.error);
} else if (process.env.TEST === '1') {
  runAutomatedTest().catch(console.error);
} else {
  // Default to REPL mode
  runRepl().catch(console.error);
}