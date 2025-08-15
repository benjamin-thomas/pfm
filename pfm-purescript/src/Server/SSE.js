// Foreign functions for SSE support
import { PassThrough } from 'stream';

// Create a PassThrough stream for SSE
export const createSSEStream = () => {
  return new PassThrough();
};

// Send an SSE message to a stream
export const sendSSEMessage = (stream) => (eventType) => (data) => () => {
  stream.write(`event: ${eventType}\n`);
  stream.write(`data: ${data}\n\n`);
};

// Start sending periodic pings
export const startPingInterval = (stream) => (intervalMs) => () => {
  const interval = setInterval(() => {
    const time = new Date().toISOString();
    stream.write('event: ping\n');
    stream.write(`data: {"time": "${time}"}\n\n`);
  }, intervalMs);

  // Return cleanup function
  return () => clearInterval(interval);
};