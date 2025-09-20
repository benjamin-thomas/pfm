import * as http from 'http';

export interface SSEClient {
    id: string;
    req: http.IncomingMessage;
    res: http.ServerResponse;
    connectedAt: Date;
}

export interface ClientStatus {
    activeConnections: number;
    clients: Array<{
        id: string;
        connectedAt: Date;
    }>;
}

export interface SSEMessage {
    type: 'connected' | 'ping' | 'refresh' | 'transaction_saved' | 'transaction_deleted';
    clientId?: string;
    message?: string;
    timestamp?: number;
    totalClients?: number;
}

const generateAlphabeticId = (n: number): string => {
    let result = '';
    let num = n;
    while (num >= 0) {
        result = String.fromCharCode(65 + (num % 26)) + result;
        num = Math.floor(num / 26) - 1;
        if (num < 0) break;
    }
    return result;
};

export interface ClientManager {
    addClient(req: http.IncomingMessage, res: http.ServerResponse): SSEClient;

    removeClient(clientId: string): void;

    getStatus(): ClientStatus;

    broadcast(message: SSEMessage): void;

    sendToClient(clientId: string, message: SSEMessage): boolean;

    getClients(): Map<string, SSEClient>;
}

export const clientManagerInit = (): ClientManager => {
    const clients = new Map<string, SSEClient>();
    let clientIdCounter = 0;

    const addClient = (req: http.IncomingMessage, res: http.ServerResponse): SSEClient => {
        const clientId = generateAlphabeticId(clientIdCounter++);
        const client: SSEClient = {
            id: clientId,
            req,
            res,
            connectedAt: new Date(),
        };
        clients.set(clientId, client);
        console.log(`[${new Date().toISOString()}] Client ${clientId} added. Total clients: ${clients.size}`);
        return client;
    };

    const removeClient = (clientId: string): void => {
        if (clients.delete(clientId)) {
            console.log(`[${new Date().toISOString()}] Client ${clientId} removed. Remaining: ${clients.size}`);
        }
    };

    const getStatus = (): ClientStatus => {
        return {
            activeConnections: clients.size,
            clients: Array.from(clients.values()).map(c => ({
                id: c.id,
                connectedAt: c.connectedAt,
            })),
        };
    };

    const broadcast = (message: SSEMessage): void => {
        const data = `data: ${JSON.stringify(message)}\n\n`;
        let successCount = 0;
        const disconnectedClients: string[] = [];

        clients.forEach((client) => {
            if (client.res.destroyed || client.res.writableEnded) {
                disconnectedClients.push(client.id);
                return;
            }
            try {
                client.res.write(data);
                successCount++;
            } catch (error) {
                console.log(`[${new Date().toISOString()}] Error broadcasting to ${client.id}:`, error);
                disconnectedClients.push(client.id);
            }
        });

        // Clean up disconnected clients
        disconnectedClients.forEach(id => removeClient(id));

        console.log(`[${new Date().toISOString()}] Broadcast to ${successCount} clients`);
    };

    const sendToClient = (clientId: string, message: SSEMessage): boolean => {
        const client = clients.get(clientId);
        if (!client) return false;

        try {
            const data = `data: ${JSON.stringify(message)}\n\n`;
            client.res.write(data);
            return true;
        } catch (error) {
            console.log(`[${new Date().toISOString()}] Error sending to ${clientId}:`, error);
            removeClient(clientId);
            return false;
        }
    };

    return {
        addClient,
        removeClient,
        getStatus,
        broadcast,
        sendToClient,
        getClients: () => clients,
    };
};

