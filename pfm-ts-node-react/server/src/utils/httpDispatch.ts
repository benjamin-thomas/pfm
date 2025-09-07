// HTTP Dispatch - pure function dispatching based on HTTP patterns
import { z, type ZodSchema } from 'zod';

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH';

// Method validation
export const isValidMethod = (method: unknown): method is HttpMethod => {
  return typeof method === 'string' && 
    ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH'].includes(method);
};

// Handler function types - pure functions that do work
type ExactHandler = () => Promise<void> | void;
type SyncParameterHandler<T> = (param: T) => void;
type AsyncParameterHandler<T> = (param: T) => Promise<void>;

// Route types
interface ExactRoute {
  type: 'exact';
  method: HttpMethod;
  pattern: string;
  handler: ExactHandler;
}

interface SyncParameterRoute<T> {
  type: 'sync-parameter';
  method: HttpMethod;
  pattern: string;
  schema: ZodSchema<T>;
  handler: SyncParameterHandler<T>;
}

interface AsyncParameterRoute<T> {
  type: 'async-parameter';
  method: HttpMethod;
  pattern: string;
  schema: ZodSchema<T>;
  handler: AsyncParameterHandler<T>;
}

type Route = ExactRoute | SyncParameterRoute<any> | AsyncParameterRoute<any>;


export const httpDispatchInit = () => {
  const routes: Route[] = [];

  const extractParam = (pattern: string, pathname: string): { matched: boolean; param: string | null } => {
    const patternParts = pattern.split('/');
    const pathParts = pathname.split('/');

    if (patternParts.length !== pathParts.length) {
      return { matched: false, param: null };
    }

    let param: string | null = null;

    for (let i = 0; i < patternParts.length; i++) {
      const patternPart = patternParts[i]!;
      const pathPart = pathParts[i]!;

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

  const onMatch = (method: HttpMethod, pattern: string, handler: ExactHandler): void => {
    routes.push({ type: 'exact', method, pattern, handler });
  };

  // Generic parameter handlers with Zod validation - schema-first approach
  const onMatchP = <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: SyncParameterHandler<T>): void => {
    routes.push({ 
      type: 'sync-parameter', 
      method, 
      pattern, 
      schema,
      handler 
    });
  };

  const onMatchP_Async = <T>(method: HttpMethod, pattern: string, schema: ZodSchema<T>, handler: AsyncParameterHandler<T>): void => {
    routes.push({ 
      type: 'async-parameter', 
      method, 
      pattern, 
      schema,
      handler 
    });
  };


  const run = async ({ method, url }: { method: HttpMethod; url: string }): Promise<boolean> => {
    const urlObj = new URL(url, 'http://localhost');
    const pathname = urlObj.pathname;

    for (const route of routes) {
      if (route.method !== method) continue;

      const result = extractParam(route.pattern, pathname);
      if (result.matched) {
        if (route.type === 'exact') {
          await route.handler();
        } else if (route.type === 'sync-parameter') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          
          // Use Zod schema to parse and validate the parameter
          const validatedParam = route.schema.parse(result.param);
          route.handler(validatedParam);
        } else if (route.type === 'async-parameter') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          
          // Use Zod schema to parse and validate the parameter
          const validatedParam = route.schema.parse(result.param);
          await route.handler(validatedParam);
        }
        return true;
      }
    }

    return false; // No route matched
  };

  // Return clean object literal without Object.assign indirection
  return {
    onMatch,
    onMatchP,
    onMatchP_Async,
    run
  };
};