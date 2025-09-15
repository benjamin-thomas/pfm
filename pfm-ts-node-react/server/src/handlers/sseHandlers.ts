import * as http from 'http';

interface Config {
  env: string;
  port: number;
  frontendUrl: string;
}

export const events = (config: Config) =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    res.writeHead(200, {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive',
      'Access-Control-Allow-Origin': config.frontendUrl,
    });

    // Send initial connection message
    res.write(`data: ${JSON.stringify({ type: 'connected', timestamp: Date.now() })}\n\n`);

    // Keep connection alive with heartbeat
    const heartbeat = setInterval(() => {
      res.write(`:heartbeat\n\n`);
    }, 30000);

    // Clean up on client disconnect
    req.on('close', () => {
      clearInterval(heartbeat);
    });
  };