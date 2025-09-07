// HTTP Dispatch - pure function dispatching based on HTTP patterns
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
type ParameterConverter<T> = (value: string) => T;

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
  converter: ParameterConverter<T>;
  handler: SyncParameterHandler<T>;
}

interface AsyncParameterRoute<T> {
  type: 'async-parameter';
  method: HttpMethod;
  pattern: string;
  converter: ParameterConverter<T>;
  handler: AsyncParameterHandler<T>;
}

type Route = ExactRoute | SyncParameterRoute<any> | AsyncParameterRoute<any>;

// Type conversion utilities
const convertParameter = <T>(value: string, targetType: 'string' | 'number'): T => {
  if (targetType === 'number') {
    const n = parseInt(value, 10);
    if (isNaN(n)) {
      throw new Error(`Invalid number parameter: ${value}`);
    }
    return n as T;
  }
  return value as T;
};

// Infer parameter type from TypeScript generic
const inferParameterType = <T>(): 'string' | 'number' => {
  // This is a bit of TypeScript magic - we'll use a runtime check
  // In practice, we could enhance this with branded types or explicit type hints
  return 'string'; // Default to string, will be overridden for number types
};

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

  // Generic parameter handlers with type conversion
  const onMatchP = <T>(method: HttpMethod, pattern: string, handler: SyncParameterHandler<T>, converter: ParameterConverter<T>): void => {
    routes.push({ 
      type: 'sync-parameter', 
      method, 
      pattern, 
      converter,
      handler 
    });
  };

  const onMatchP_Async = <T>(method: HttpMethod, pattern: string, handler: AsyncParameterHandler<T>, converter: ParameterConverter<T>): void => {
    routes.push({ 
      type: 'async-parameter', 
      method, 
      pattern, 
      converter,
      handler 
    });
  };

  // Helper function to determine parameter type from the first call
  const determineParameterType = (value: string): 'string' | 'number' => {
    const num = parseInt(value, 10);
    return !isNaN(num) && num.toString() === value ? 'number' : 'string';
  };

  const dispatch = async ({ method, url }: { method: HttpMethod; url: string }): Promise<boolean> => {
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
          
          // Use the stored converter function
          const convertedParam = route.converter(result.param);
          route.handler(convertedParam);
        } else if (route.type === 'async-parameter') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          
          // Use the stored converter function
          const convertedParam = route.converter(result.param);
          await route.handler(convertedParam);
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
    dispatch
  };
};