// HTTP Dispatch v2 - Trie-based O(1) routing with IO abstraction
// Based on the OCaml trie3.ml implementation

import { type ZodType } from 'zod';

// ======================================================================
// Core Types
// ======================================================================

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH' | 'HEAD' | 'TRACE' | 'CONNECT';


// ======================================================================
// HANDLER TYPES - THE CORE OF THE PATTERN
// ======================================================================
// These types encode the "handler creates context" pattern:
//
// P0Handler: Ctx
//   - No URL parameters
//   - Returns execution context directly
//   - Example: healthCheck -> (req, res) => sendJson(res, 200, {...})
//
// P1Handler: (param) => Ctx
//   - One URL parameter (e.g., /users/:id)
//   - Takes parameter, returns execution context
//   - Example: getUser(id) -> (req, res) => sendJson(res, 200, user)
//
// P2Handler: (param1, param2) => Ctx
//   - Two URL parameters (e.g., /users/:id/posts/:postId)
//   - Takes both parameters, returns execution context
//
// The key insight: Ctx is the execution context - a function that knows how
// to process an HTTP request. It can be sync or async, the router doesn't care.
//
type P0Handler<Ctx> = Ctx;
type P1Handler<Ctx> = (param: unknown) => Ctx;
type P2Handler<Ctx> = (param1: unknown, param2: unknown) => Ctx;

// Route action - stores handler with its decoder(s)
type RouteAction<Ctx> =
  | { tag: 'P0'; handler: P0Handler<Ctx> }
  | { tag: 'P1'; schema: ZodType<unknown>; handler: P1Handler<Ctx> }
  | { tag: 'P2'; schema1: ZodType<unknown>; schema2: ZodType<unknown>; handler: P2Handler<Ctx> };


// ======================================================================
// Trie-based Route Dispatcher
// ======================================================================

// Trie node structure - similar to trie3.ml approach
type TrieNode<Ctx> =
  | { type: 'children'; children: Map<string, TrieNode<Ctx>> }
  | { type: 'exec'; handlers: Map<HttpMethod, RouteAction<Ctx>> };

export interface HttpDispatcher<Ctx> {
  matchP0: (method: HttpMethod, pattern: string, handler: P0Handler<Ctx>) => void;
  matchP1: <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, handler: (param: T) => Ctx) => void;
  matchP2: <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, handler: (param1: T, param2: U) => Ctx) => void;
  run: <T>(method: string, path: string, executor: (ctx: Ctx) => T) => T;
}

// ======================================================================
// ERROR CALLBACKS - THEY FOLLOW THE SAME PATTERN!
// ======================================================================
// Even error handling follows the "handler creates context" pattern.
// Instead of performing side effects directly:
//   onError: (ctx, msg) => sendError(ctx.res, msg)  // old way
//
// Error callbacks return execution contexts:
//   onError: (msg) => (req, res) => sendError(res, msg)  // new way
//
// This keeps the pattern consistent throughout the system.
//
interface DispatchCallbacks<Ctx> {
  onMatchBadParam: (message: string) => Ctx;
  onMatchNotFound: (message: string) => Ctx;
}

export const httpDispatch2Init = <Ctx>(callbacks: DispatchCallbacks<Ctx>): HttpDispatcher<Ctx> => {
  const root: TrieNode<Ctx> = { type: 'children', children: new Map() };

  // Split path into segments
  const splitPath = (path: string): string[] => {
    return path.split('/').filter(s => s !== '');
  };

  // Add a route to the trie
  const addRoute = (method: HttpMethod, pattern: string, action: RouteAction<Ctx>): void => {
    const segments = splitPath(pattern);

    const insertNode = (node: TrieNode<Ctx>, remainingSegments: string[]): void => {
      if (remainingSegments.length === 0) {
        // At the end of path - store the handler
        if (node.type === 'exec') {
          node.handlers.set(method, action);
        } else {
          // Store handler in empty string key within children node
          let execNode = node.children.get('');
          if (!execNode || execNode.type !== 'exec') {
            execNode = { type: 'exec', handlers: new Map() };
            node.children.set('', execNode);
          }
          execNode.handlers.set(method, action);
        }
        return;
      }

      const [segment, ...rest] = remainingSegments;
      if (segment === undefined) {
        throw new Error('Empty segment in route pattern');
      }

      if (node.type === 'exec') {
        throw new Error('Cannot add child to exec node');
      }

      let nextNode = node.children.get(segment);
      if (!nextNode) {
        // Always create children nodes to allow both terminal and continuation
        nextNode = { type: 'children', children: new Map() };
        node.children.set(segment, nextNode);
      }

      insertNode(nextNode, rest);
    };

    insertNode(root, segments);
  };

  // Register routes with different parameter counts
  const matchP0 = (method: HttpMethod, pattern: string, handler: P0Handler<Ctx>): void => {
    addRoute(method, pattern, { tag: 'P0', handler });
  };

  const matchP1 = <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, handler: (param: T) => Ctx): void => {
    addRoute(method, pattern, { tag: 'P1', schema: schema as ZodType<unknown>, handler: handler as P1Handler<Ctx> });
  };

  const matchP2 = <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, handler: (param1: T, param2: U) => Ctx): void => {
    addRoute(method, pattern, { tag: 'P2', schema1: schema1 as ZodType<unknown>, schema2: schema2 as ZodType<unknown>, handler: handler as P2Handler<Ctx> });
  };

  // ======================================================================
  // EXECUTE ACTION - WHERE THE MAGIC HAPPENS
  // ======================================================================
  // This function embodies the core pattern:
  // 1. Call handler to get execution context: action.handler(...params)
  // 2. Pass execution context to executor: executor(ctx)
  // 3. Executor decides how to run the context (usually with req/res)
  //
  // Note: No Promise.resolve() wrapping! The execution context itself
  // carries whether it's sync or async. We just pass through whatever
  // the executor returns (T can be void, Promise<void>, or anything else).
  //
  const executeAction = <T>(action: RouteAction<Ctx>, params: string[], executor: (ctx: Ctx) => T): T => {
    switch (action.tag) {
      case 'P0':
        if (params.length !== 0) {
          return executor(callbacks.onMatchBadParam('Invalid route configuration: P0 with parameters'));
        }
        return executor(action.handler);


      case 'P1':
        if (params.length !== 1) {
          return executor(callbacks.onMatchBadParam(`Invalid route configuration: expected 1 parameter, got ${params.length}`));
        }
        try {
          const value = action.schema.parse(params[0]);
          return executor(action.handler(value));
        } catch {
          return executor(callbacks.onMatchBadParam(`Decoding error: invalid parameter '${params[0]}'`));
        }


      case 'P2':
        if (params.length !== 2) {
          return executor(callbacks.onMatchBadParam(`Invalid route configuration: expected 2 parameters, got ${params.length}`));
        }
        try {
          const value1 = action.schema1.parse(params[0]);
          const value2 = action.schema2.parse(params[1]);
          return executor(action.handler(value1, value2));
        } catch {
          return executor(callbacks.onMatchBadParam(`Decoding error: invalid parameters '${params[0]}', '${params[1]}'`));
        }
    }
  };

  const isValidMethod = (method: string): method is HttpMethod => {
    return ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH', 'HEAD', 'TRACE', 'CONNECT'].includes(method);
  };

  // ======================================================================
  // RUN - THE ENTRY POINT
  // ======================================================================
  // The public interface of our "handler creates context" router:
  //
  // Usage: router.run(method, path, (execCtx) => execCtx(req, res))
  //
  // 1. Find handler for method/path
  // 2. Get execution context from handler
  // 3. Pass context to executor
  // 4. Return whatever executor returns (sync or async)
  //
  // The executor is where the caller decides HOW to run the context.
  // In production: (execCtx) => execCtx(req, res)
  // In tests: (execCtx) => execCtx()  // for NOOP contexts
  //
  const run = <T>(method: string, path: string, executor: (ctx: Ctx) => T): T => {
    // Validate method
    if (!isValidMethod(method)) {
      return executor(callbacks.onMatchBadParam(`Unsupported method '${method}'`));
    }

    const segments = splitPath(path);
    const capturedParams: string[] = [];

    const findHandler = (node: TrieNode<Ctx>, remainingSegments: string[]): RouteAction<Ctx> | null => {
      if (remainingSegments.length === 0) {
        // At the end of path - check for handler
        if (node.type === 'exec') {
          return node.handlers.get(method as HttpMethod) ?? null;
        } else {
          // Look for handler at empty string key
          const execNode = node.children.get('');
          if (execNode && execNode.type === 'exec') {
            return execNode.handlers.get(method as HttpMethod) ?? null;
          }
          return null;
        }
      }

      if (node.type === 'exec') {
        return null;
      }

      const [segment, ...rest] = remainingSegments;
      if (segment === undefined) {
        return null;
      }

      // Try exact match first
      const exactNode = node.children.get(segment);
      if (exactNode) {
        const result = findHandler(exactNode, rest);
        if (result) return result;
      }

      // Try wildcard match
      const wildcardNode = node.children.get('?');
      if (wildcardNode) {
        capturedParams.push(segment);
        return findHandler(wildcardNode, rest);
      }

      return null;
    };

    const action = findHandler(root, segments);
    if (action) {
      return executeAction(action, capturedParams, executor);
    } else {
      return executor(callbacks.onMatchNotFound(`No route found for ${method} ${path}`));
    }
  };

  return {
    matchP0,
    matchP1,
    matchP2,
    run,
  };
};

