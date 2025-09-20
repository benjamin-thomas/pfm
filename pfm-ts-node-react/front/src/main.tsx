import { StrictMode } from 'react';
import { createRoot } from 'react-dom/client';
import { Provider } from 'react-redux';
import './main.css';
import App from './App.tsx';
import SharedWorkerUrl from './workers/sse-worker.ts?sharedworker&url';
import { store } from './store';
import { sseActions } from './redux/sseUpdate.ts';
import * as financialEffects from './redux/financialEffects';
import type { SSEMessage } from './redux/sseUpdate.ts';

// Extract API URL once at startup
const apiUrl = import.meta.env.VITE_API_URL;
if (!apiUrl) {
  throw new Error(
    'VITE_API_URL environment variable is required. ' +
    'Run with: VITE_API_URL=http://localhost:8083 npm run dev'
  );
}

// Initialize SharedWorker for SSE before React starts
console.log('[PRE-APP] Initializing SSE SharedWorker...');

const worker = new SharedWorker(SharedWorkerUrl, { type: 'module' });

// Connect SSE to Redux
worker.port.onmessage = (event: MessageEvent): void => {
  const message: SSEMessage = event.data;

  console.log("[PRE-APP]", message);

  // Dispatch to Redux store
  store.dispatch(sseActions.rcvSseMessage(message));

  // Handle refresh requests
  if (message.kind === 'shouldRefresh') {
    console.log('[PRE-APP] Server requested refresh');
    financialEffects.fetchData(apiUrl, store.dispatch);
  }
};

worker.onerror = (error) => {
  console.error('[Infrastructure] SharedWorker error:', error);
};

// Start the worker port
worker.port.start();

// Initial data fetch
financialEffects.fetchData(apiUrl, store.dispatch);

// Now start React with Redux
const root = document.getElementById('root');
if (!root) throw new Error('Root element not found');

createRoot(root).render(
  <StrictMode>
    <Provider store={store}>
      <App apiUrl={apiUrl} />
    </Provider>
  </StrictMode>
);
