// HTTP Dispatch - pure function dispatching based on HTTP patterns
import { type ZodType } from 'zod';

export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH';

// Method validation
export const isValidMethod = (method: unknown): method is HttpMethod => {
  return typeof method === 'string' &&
    ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH'].includes(method);
};

// Route types - simplified with async-only handlers
interface BaseRoute {
  method: HttpMethod;
  pattern: string;
  execution:
  | { tag: 'exact'; toPromise: () => Promise<void> }
  | { tag: 'param1'; schema: ZodType<unknown>; toPromise: (param: unknown) => Promise<void> }
  | { tag: 'param2'; schema1: ZodType<unknown>; schema2: ZodType<unknown>; toPromise: (param1: unknown, param2: unknown) => Promise<void> };
}

type Route = BaseRoute;


export const httpDispatchInit = (): {
  matchP0: (method: HttpMethod, pattern: string, toPromise: () => Promise<void>) => void;
  matchP1: <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, toPromise: (param: T) => Promise<void>) => void;
  matchP2: <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, toPromise: (param1: T, param2: U) => Promise<void>) => void;
  run: (request: { method: HttpMethod; url: string }) => Promise<boolean>;
} => {
  const routes: Route[] = [];

  const extractParams = (pattern: string, pathname: string): { matched: boolean; params: string[] } => {
    const patternParts = pattern.split('/');
    const pathParts = pathname.split('/');

    if (patternParts.length !== pathParts.length) {
      return { matched: false, params: [] };
    }

    const params: string[] = [];

    for (const [i, patternPart] of patternParts.entries()) {
      const pathPart = pathParts[i];
      if (!pathPart) continue;

      if (patternPart === '?') {
        // This is a parameter, capture it
        params.push(pathPart);
      } else if (patternPart !== pathPart) {
        // Static part doesn't match
        return { matched: false, params: [] };
      }
    }

    return { matched: true, params }; // params array contains captured values
  };

  const matchP0 = (method: HttpMethod, pattern: string, toPromise: () => Promise<void>): void => {
    routes.push({ method, pattern, execution: { tag: 'exact', toPromise } });
  };

  // Generic parameter handlers with Zod validation - schema-first approach
  const matchP1 = <T>(method: HttpMethod, pattern: string, schema: ZodType<T>, toPromise: (param: T) => Promise<void>): void => {
    routes.push({
      method,
      pattern,
      execution: { tag: 'param1', schema: schema as ZodType<unknown>, toPromise: toPromise as (param: unknown) => Promise<void> }
    });
  };

  const matchP2 = <T, U>(method: HttpMethod, pattern: string, schema1: ZodType<T>, schema2: ZodType<U>, toPromise: (param1: T, param2: U) => Promise<void>): void => {
    routes.push({
      method,
      pattern,
      execution: { tag: 'param2', schema1: schema1 as ZodType<unknown>, schema2: schema2 as ZodType<unknown>, toPromise: toPromise as (param1: unknown, param2: unknown) => Promise<void> }
    });
  };


  const run = async ({ method, url }: { method: HttpMethod; url: string }): Promise<boolean> => {
    const urlObj = new URL(url, 'http://localhost');
    const pathname = urlObj.pathname;

    for (const route of routes) {
      if (route.method !== method) continue;

      const result = extractParams(route.pattern, pathname);
      if (result.matched) {
        // Execute promise based on route type
        switch (route.execution.tag) {
          case 'exact':
            await route.execution.toPromise();
            break;
          case 'param1':
            if (result.params.length !== 1) {
              throw new Error('Expected exactly 1 parameter but found ' + result.params.length);
            }
            const validatedParam = route.execution.schema.parse(result.params[0]);
            await route.execution.toPromise(validatedParam);
            break;
          case 'param2':
            if (result.params.length !== 2) {
              throw new Error('Expected exactly 2 parameters but found ' + result.params.length);
            }
            const validatedParam1 = route.execution.schema1.parse(result.params[0]);
            const validatedParam2 = route.execution.schema2.parse(result.params[1]);
            await route.execution.toPromise(validatedParam1, validatedParam2);
            break;
          default:
            // noinspection UnnecessaryLocalVariableJS
            const _exhaustiveCheck: never = route.execution;
            return _exhaustiveCheck;
        }
        return true;
      }
    }

    return false; // No route matched
  };

  // Return clean object literal without Object.assign indirection
  return {
    matchP0,
    matchP1,
    matchP2,
    run
  };
};