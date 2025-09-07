// Dispatch - pure function dispatching based on patterns
export type HttpMethod = 'GET' | 'POST' | 'PUT' | 'DELETE' | 'OPTIONS' | 'PATCH';

// Method validation
export const isValidMethod = (method: unknown): method is HttpMethod => {
  return typeof method === 'string' && 
    ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'PATCH'].includes(method);
};

// Handler function types - pure functions that do work
type ExactHandler = () => Promise<void> | void;
type StringHandler = (param: string) => Promise<void> | void;
type NumberHandler = (param: number) => Promise<void> | void;

// Route types
interface ExactRoute {
  type: 'exact';
  method: HttpMethod;
  pattern: string;
  handler: ExactHandler;
}

interface StringParameterizedRoute {
  type: 'string-parameterized';
  method: HttpMethod;
  pattern: string;
  handlerFactory: (param: string) => StringHandler;
}

interface NumberParameterizedRoute {
  type: 'number-parameterized';
  method: HttpMethod;
  pattern: string;
  handlerFactory: (param: number) => NumberHandler;
}

type Route = ExactRoute | StringParameterizedRoute | NumberParameterizedRoute;

export const dispatchInit = () => {
  const routes: Route[] = [];

  const onMatch = (method: HttpMethod, pattern: string, handler: ExactHandler): void => {
    routes.push({ type: 'exact', method, pattern, handler });
  };

  const onMatchPstring = (method: HttpMethod, pattern: string, handlerFactory: (param: string) => StringHandler): void => {
    routes.push({ type: 'string-parameterized', method, pattern, handlerFactory });
  };

  const onMatchPnumber = (method: HttpMethod, pattern: string, handlerFactory: (param: number) => NumberHandler): void => {
    routes.push({ type: 'number-parameterized', method, pattern, handlerFactory });
  };

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

  // Pure dispatch function - just takes method and url
  const dispatch = async ({ method, url }: { method: HttpMethod; url: string }): Promise<boolean> => {
    const urlObj = new URL(url, 'http://localhost');
    const pathname = urlObj.pathname;

    for (const route of routes) {
      if (route.method !== method) continue;

      const result = extractParam(route.pattern, pathname);
      if (result.matched) {
        if (route.type === 'exact') {
          await route.handler();
        } else if (route.type === 'string-parameterized') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          const handler = route.handlerFactory(result.param);
          await handler(result.param);
        } else if (route.type === 'number-parameterized') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          const n = parseInt(result.param, 10);
          if (isNaN(n)) {
            throw new Error(`Invalid number parameter: ${result.param}`);
          }
          const handler = route.handlerFactory(n);
          await handler(n);
        }
        return true;
      }
    }

    return false; // No route matched
  };

  // Return dispatch function with registration methods
  return Object.assign(dispatch, { onMatch, onMatchPstring, onMatchPnumber });
};