import * as http from 'http';

interface Config {
  env: string;
  port: number;
  frontendUrl: string;
}

// JSON response helper
const sendJson = (res: http.ServerResponse, statusCode: number, data: unknown): void => {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json');
  res.end(JSON.stringify(data));
};

export const check = async (req: http.IncomingMessage, res: http.ServerResponse, config: Config): Promise<void> => {
  sendJson(res, 200, { status: 'ok', env: config.env, port: config.port });
};

export const hello = async (req: http.IncomingMessage, res: http.ServerResponse, name: string): Promise<void> => {
  sendJson(res, 200, { message: `Hello, ${name}!` });
};