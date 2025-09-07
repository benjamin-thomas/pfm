import * as http from 'http';

// Handler function that takes (req, res) and optionally returns a promise
type HandlerFunction = (req: http.IncomingMessage, res: http.ServerResponse) => Promise<void> | void;

// Route types
interface ExactRoute {
  type: 'exact';
  method: string;
  pattern: string;
  handler: HandlerFunction;
}

interface StringParameterizedRoute {
  type: 'string-parameterized';
  method: string;
  pattern: string;
  handlerFactory: (param: string) => HandlerFunction;
}

interface NumberParameterizedRoute {
  type: 'number-parameterized';
  method: string;
  pattern: string;
  handlerFactory: (param: number) => HandlerFunction;
}

type Route = ExactRoute | StringParameterizedRoute | NumberParameterizedRoute;

export const createRouter = () => {
  const routes: Route[] = [];

  const onMatch = (method: string, pattern: string, handler: HandlerFunction): void => {
    routes.push({ type: 'exact', method, pattern, handler });
  };

  const onMatchPstring = (method: string, pattern: string, handlerFactory: (param: string) => HandlerFunction): void => {
    routes.push({ type: 'string-parameterized', method, pattern, handlerFactory });
  };

  const onMatchPnumber = (method: string, pattern: string, handlerFactory: (param: number) => HandlerFunction): void => {
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

  const run = async (req: http.IncomingMessage, res: http.ServerResponse): Promise<boolean> => {
    // Fail early if we don't have basic info
    if (!req.method) {
      throw new Error('Request method is required');
    }

    if (!req.url) {
      throw new Error('Request URL is required');
    }

    const method = req.method;
    const url = new URL(req.url, 'http://localhost');
    const pathname = url.pathname;

    for (const route of routes) {
      if (route.method !== method) continue;

      const result = extractParam(route.pattern, pathname);
      if (result.matched) {
        if (route.type === 'exact') {
          await route.handler(req, res);
        } else if (route.type === 'string-parameterized') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          const handler = route.handlerFactory(result.param);
          await handler(req, res);
        } else if (route.type === 'number-parameterized') {
          if (result.param === null) {
            throw new Error('Expected parameter but none found');
          }
          const n = parseInt(result.param, 10);
          if (isNaN(n)) {
            throw new Error(`Invalid number parameter: ${result.param}`);
          }
          const handler = route.handlerFactory(n);
          await handler(req, res);
        }
        return true;
      }
    }

    return false; // No route matched
  };

  return { onMatch, onMatchPstring, onMatchPnumber, run };
};