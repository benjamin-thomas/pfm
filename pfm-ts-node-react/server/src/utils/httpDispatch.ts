// HTTP Dispatch - pure function dispatching based on HTTP patterns
import { type ZodSchema } from 'zod';

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH';

// Method validation
export const isValidMethod = (method: unknown): method is HttpMethod => {
  return typeof method === 'string' &&
    ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH'].includes(method);
};

// Handler function types - pure functions that do work
type SyncHandler = () => void;
type AsyncHandler = () => Promise<void>;
type SyncParameterHandler<T> = (param: T) => void;
type AsyncParameterHandler<T> = (param: T) => Promise<void>;

// Route types with nested discriminated union - groups common fields
interface BaseRoute {
  method: HttpMethod;
  pattern: string;
  execution:
  | { kind: 'sync'; tag: 'exact'; handler: SyncHandler }
  | { kind: 'async'; tag: 'exact'; handler: AsyncHandler }
  | { kind: 'sync'; tag: 'param'; schema: ZodSchema<unknown>; handler: SyncParameterHandler<unknown> }
  | { kind: 'async'; tag: 'param'; schema: ZodSchema<unknown>; handler: AsyncParameterHandler<unknown> };
}

type Route = BaseRoute;


export const httpDispatchInit = (): {
  onMatchSync: (method: HttpMethod, pattern: string, handler: SyncHandler) => void;
  onMatchAsync: (method: HttpMethod, pattern: string, handler: AsyncHandler) => void;
  onMatchP_Sync: <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: SyncParameterHandler<T>) => void;
  onMatchP_Async: <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: AsyncParameterHandler<T>) => void;
  run: (request: { method: HttpMethod; url: string }) => Promise<boolean>;
} => {
  const routes: Route[] = [];

  const extractParam = (pattern: string, pathname: string): { matched: boolean; param: string | null } => {
    const patternParts = pattern.split('/');
    const pathParts = pathname.split('/');

    if (patternParts.length !== pathParts.length) {
      return { matched: false, param: null };
    }

    let param: string | null = null;

    for (const [i, patternPart] of patternParts.entries()) {
      const pathPart = pathParts[i];
      if (!pathPart) continue;


      if (patternPart.startsWith('{') && patternPart.endsWith('}')) {
        // This is a parameter, capture it
        param = pathPart;
      } else if (patternPart !== pathPart) {
        // Static part doesn't match
        return { matched: false, param: null };
      }
    }

    return { matched: true, param }; // param is null for exact matches, string for param matches
  };

  const onMatchSync = (method: HttpMethod, pattern: string, handler: SyncHandler): void => {
    routes.push({ method, pattern, execution: { kind: 'sync', tag: 'exact', handler } });
  };

  const onMatchAsync = (method: HttpMethod, pattern: string, handler: AsyncHandler): void => {
    routes.push({ method, pattern, execution: { kind: 'async', tag: 'exact', handler } });
  };

  // Generic parameter handlers with Zod validation - schema-first approach
  const onMatchP_Sync = <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: SyncParameterHandler<T>): void => {
    routes.push({
      method,
      pattern,
      execution: { kind: 'sync', tag: 'param', schema: schema as ZodSchema<unknown>, handler: handler as SyncParameterHandler<unknown> }
    });
  };

  const onMatchP_Async = <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: AsyncParameterHandler<T>): void => {
    routes.push({
      method,
      pattern,
      execution: { kind: 'async', tag: 'param', schema: schema as ZodSchema<unknown>, handler: handler as AsyncParameterHandler<unknown> }
    });
  };


  const run = async ({ method, url }: { method: HttpMethod; url: string }): Promise<boolean> => {
    const urlObj = new URL(url, 'http://localhost');
    const pathname = urlObj.pathname;

    for (const route of routes) {
      if (route.method !== method) continue;

      const result = extractParam(route.pattern, pathname);
      if (result.matched) {
        // Clean pattern matching with nested switches on execution
        switch (route.execution.kind) {
          case 'sync':
            switch (route.execution.tag) {
              case 'exact':
                route.execution.handler();
                break;
              case 'param':
                if (result.param === null) {
                  throw new Error('Expected parameter but none found');
                }
                const syncValidatedParam = route.execution.schema.parse(result.param);
                route.execution.handler(syncValidatedParam);
                break;
              default:
                const _exhaustiveCheckSync: never = route.execution;
                return _exhaustiveCheckSync;
            }
            break;
          case 'async':
            switch (route.execution.tag) {
              case 'exact':
                await route.execution.handler();
                break;
              case 'param':
                if (result.param === null) {
                  throw new Error('Expected parameter but none found');
                }
                const asyncValidatedParam = route.execution.schema.parse(result.param);
                await route.execution.handler(asyncValidatedParam);
                break;
              default:
                const _exhaustiveCheckAsync: never = route.execution;
                return _exhaustiveCheckAsync;
            }
            break;
          default:
            const _exhaustiveCheckKind: never = route.execution;
            return _exhaustiveCheckKind;
        }
        return true;
      }
    }

    return false; // No route matched
  };

  // Return clean object literal without Object.assign indirection
  return {
    onMatchSync,
    onMatchAsync,
    onMatchP_Sync,
    onMatchP_Async,
    run
  };
};