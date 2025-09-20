import * as http from 'http';
import type { ClientManager } from '../services/clientManager';

interface Config {
  env: string;
  port: number;
  frontendUrl: string;
}

export const events = (config: Config, clientManager: ClientManager) =>
  async (req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    console.log(`[${new Date().toISOString()}] Client connecting...`);

    // Set SSE headers
    res.writeHead(200, {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive',
      'Access-Control-Allow-Origin': config.frontendUrl,
    });

    // Add client to manager
    const client = clientManager.addClient(req, res);
    console.log(`[${new Date().toISOString()}] Client ${client.id} connected. Total clients: ${clientManager.getStatus().activeConnections}`);

    // Send initial connection message
    res.write(`data: ${JSON.stringify({
      type: 'connected',
      clientId: client.id,
      message: 'Connection established',
      timestamp: Date.now()
    })}\n\n`);

    // Cleanup function that accepts the interval as a parameter
    const cleanup = (interval: NodeJS.Timeout): void => {
      console.log(`[${new Date().toISOString()}] Cleaning up client ${client.id}`);
      clearInterval(interval);
      clientManager.removeClient(client.id);

      try {
        if (!res.destroyed && !res.writableEnded) {
          res.end();
        }
      } catch (e) {
        console.log(`[${new Date().toISOString()}] Error ending response for ${client.id}:`, e);
      }
    };

    /**
     * Send periodic pings
     * 1. setInterval assigns an ID to pingInterval
     * 2. the thunk executes by which time pingInterval exists
     */
    const pingInterval = setInterval(() => {
      if (!clientManager.getClients().has(client.id)) {
        clearInterval(pingInterval);
        return;
      }

      try {
        const pingData = {
          type: 'ping',
          timestamp: Date.now(),
          clientId: client.id,
          totalClients: clientManager.getStatus().activeConnections,
        };
        res.write(`data: ${JSON.stringify(pingData)}\n\n`);
        console.log(`[${new Date().toISOString()}] Ping sent to ${client.id}`);
      } catch (error) {
        console.log(`[${new Date().toISOString()}] Error sending ping to ${client.id}:`, error);
        cleanup(pingInterval);
      }
    }, 5000);

    // CRITICAL: These event listeners enable proper disconnection detection

    // Most reliable for detecting client disconnection
    req.on('close', () => {
      console.log(`[${new Date().toISOString()}] Request 'close' event for ${client.id}`);
      cleanup(pingInterval);
    });

    // Fired when request ends normally
    req.on('end', () => {
      console.log(`[${new Date().toISOString()}] Request 'end' event for ${client.id}`);
      cleanup(pingInterval);
    });

    // Response close event
    res.on('close', () => {
      console.log(`[${new Date().toISOString()}] Response 'close' event for ${client.id}`);
      cleanup(pingInterval);
    });

    // Response error
    res.on('error', (error) => {
      console.log(`[${new Date().toISOString()}] Response error for ${client.id}:`, error);
      cleanup(pingInterval);
    });

    // Connection abort (older Node.js versions)
    req.on('aborted', () => {
      console.log(`[${new Date().toISOString()}] Request aborted for ${client.id}`);
      cleanup(pingInterval);
    });
  };

export const status = (clientManager: ClientManager) =>
  async (_req: http.IncomingMessage, res: http.ServerResponse): Promise<void> => {
    res.writeHead(200, {
      'Content-Type': 'application/json',
      'Access-Control-Allow-Origin': '*',
    });

    const statusData = clientManager.getStatus();
    res.end(JSON.stringify(statusData, null, 2));
  };