/**
 * SharedWorker for managing SSE connections across multiple browser tabs
 * Only one actual SSE connection is created per domain, shared across all tabs
 *
 * Key concepts:
 * - MessagePort: A two-way communication channel between a tab and the SharedWorker
 * - Each tab gets its own MessagePort when it connects
 * - The 'connect' event fires for EVERY tab that connects, not just the first
 * - MessagePort has no 'close' event (platform limitation)
 *
 * @see https://developer.mozilla.org/en-US/docs/Web/API/SharedWorker
 * @see https://developer.mozilla.org/en-US/docs/Web/API/MessagePort
 * @see https://github.com/whatwg/html/issues/1766 (MessagePort close event issue)
 */

/// <reference lib="webworker" />

declare const self: SharedWorkerGlobalScope;

console.log('[SSE Worker] SharedWorker started');

// One MessagePort per connected tab
const connectedPorts: MessagePort[] = [];

// Extract API URL once at startup - fail fast if missing
const apiUrl = import.meta.env.VITE_API_URL as string;
if (!apiUrl) throw new Error('[SSE Worker] VITE_API_URL is not set!');

const broadcastToAllTabs = (message: unknown): void => {
  // MessagePort has no 'close' event, so we detect dead ports by
  // trying to send a message and catching the error.
  // Iterate backwards so we can safely splice during iteration.
  for (let i = connectedPorts.length - 1; i >= 0; i--) {
    const port = connectedPorts[i]!; // eslint-disable-line @typescript-eslint/no-non-null-assertion

    try {
      port.postMessage(message);
    } catch {
      console.log('[SSE Worker] Removing disconnected port');
      connectedPorts.splice(i, 1);
    }
  }
};

const subscribeToServerEvents = (): void => {
  console.log('[SSE Worker] Initializing SSE connection...');

  const eventSource = new EventSource(`${apiUrl}/api/events`);

  eventSource.onopen = () => {
    console.log('[SSE Worker] SSE connection opened');
  };

  eventSource.onmessage = (event) => {
    try {
      const serverMessage = JSON.parse(event.data);
      broadcastToAllTabs(serverMessage);
    } catch (error) {
      console.error('[SSE Worker] Error parsing server message:', error);
    }
  };

  eventSource.onerror = (error) => {
    console.error('[SSE Worker] SSE error:', error);

    // The EventSource will automatically try to reconnect
    // We don't need to manually handle reconnection
  };
};

// This event fires for EVERY tab that connects to the SharedWorker
self.addEventListener('connect', (event) => {
  const port = event.ports[0];
  if (!port) throw new Error("Impossible!"); // stupid API, we *always* receive a singleton array

  // Initialize SSE connection if this is the first tab
  if (connectedPorts.length === 0) subscribeToServerEvents();
  connectedPorts.push(port);

  console.log(`[SSE Worker] Tab connected. Total tabs: ${connectedPorts.length}`);
  port.start();

  // NOTE: MessagePort has no 'close' event. Dead ports are only detected
  // when we try to send them a message (in broadcastToAllTabs).
});