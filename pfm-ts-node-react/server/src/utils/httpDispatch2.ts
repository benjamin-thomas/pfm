// HTTP Dispatch v2 - Trie-based O(1) routing with IO abstraction
// Based on the OCaml trie3.ml implementation

import { type ZodType } from 'zod';

// ======================================================================
// Core Types
// ======================================================================

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH' | 'HEAD' | 'TRACE' | 'CONNECT';


// Handler types - pure functions returning promises
type P0Handler<Ctx> = (ctx: Ctx) => Promise<void>;
type P1Handler<Ctx> = (param: unknown) => (ctx: Ctx) => Promise<void>;
type P2Handler<Ctx> = (param1: unknown, param2: unknown) => (ctx: Ctx) => Promise<void>;

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
  matchP1: <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, handler: (param: T) => (ctx: Ctx) => Promise<void>) => void;
  matchP2: <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, handler: (param1: T, param2: U) => (ctx: Ctx) => Promise<void>) => void;
  run: (ctx: Ctx, method: string, path: string) => Promise<void>;
}

interface DispatchCallbacks<Ctx> {
  onMatchBadParam: (ctx: Ctx, message: string) => void;
  onMatchNotFound: (ctx: Ctx, message: string) => void;
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

  const matchP1 = <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, handler: (param: T) => (ctx: Ctx) => Promise<void>): void => {
    addRoute(method, pattern, { tag: 'P1', schema: schema as ZodType<unknown>, handler: handler as P1Handler<Ctx> });
  };

  const matchP2 = <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, handler: (param1: T, param2: U) => (ctx: Ctx) => Promise<void>): void => {
    addRoute(method, pattern, { tag: 'P2', schema1: schema1 as ZodType<unknown>, schema2: schema2 as ZodType<unknown>, handler: handler as P2Handler<Ctx> });
  };

  // Execute a route action with captured parameters
  const executeAction = async (action: RouteAction<Ctx>, params: string[], ctx: Ctx): Promise<void> => {
    switch (action.tag) {
      case 'P0':
        if (params.length !== 0) {
          callbacks.onMatchBadParam(ctx, 'Invalid route configuration: P0 with parameters');
          return;
        }
        await action.handler(ctx);
        break;

      case 'P1':
        if (params.length !== 1) {
          callbacks.onMatchBadParam(ctx, `Invalid route configuration: expected 1 parameter, got ${params.length}`);
          return;
        }
        try {
          const value = action.schema.parse(params[0]);
          await action.handler(value)(ctx);
        } catch {
          callbacks.onMatchBadParam(ctx, `Decoding error: invalid parameter '${params[0]}'`);
        }
        break;

      case 'P2':
        if (params.length !== 2) {
          callbacks.onMatchBadParam(ctx, `Invalid route configuration: expected 2 parameters, got ${params.length}`);
          return;
        }
        try {
          const value1 = action.schema1.parse(params[0]);
          const value2 = action.schema2.parse(params[1]);
          await action.handler(value1, value2)(ctx);
        } catch {
          callbacks.onMatchBadParam(ctx, `Decoding error: invalid parameters '${params[0]}', '${params[1]}'`);
        }
        break;
    }
  };

  const isValidMethod = (method: string): method is HttpMethod => {
    return ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH', 'HEAD', 'TRACE', 'CONNECT'].includes(method);
  };

  // Dispatch a request - pure side-effects through callbacks
  const run = async (ctx: Ctx, method: string, path: string): Promise<void> => {
    try {
      // Validate method
      if (!isValidMethod(method)) {
        callbacks.onMatchBadParam(ctx, `Unsupported method '${method}'`);
        return;
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
        await executeAction(action, capturedParams, ctx);
      } else {
        callbacks.onMatchNotFound(ctx, `No route found for ${method} ${path}`);
      }
    } catch (error) {
      callbacks.onMatchBadParam(ctx, error instanceof Error ? error.message : String(error));
    }
  };

  return {
    matchP0,
    matchP1,
    matchP2,
    run,
  };
};

