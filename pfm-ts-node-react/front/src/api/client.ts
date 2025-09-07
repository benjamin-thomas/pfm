import type { Transaction, NewTransaction, UpdateTransaction } from '@shared/types';

// Configuration based on environment
const getApiUrl = (): string => {
  // Detect if we're on port 4003 (dev) or 4004 (test)
  const port = window.location.port;
  if (port === '4003') {
    return 'http://localhost:8083';
  } else if (port === '4004') {
    return 'http://localhost:8084';
  }
  // Default to dev
  return 'http://localhost:8083';
};

// Helper to build full API URL
const apiUrl = (path: string): string => `${getApiUrl()}${path}`;

// Helper to parse JSON response and throw on HTTP errors
const jsonAsync = async <T>(response: Response): Promise<T> => {
  if (!response.ok) {
    let errorMessage = `HTTP ${response.status}: ${response.statusText}`;
    try {
      const errorData = await response.json();
      if (errorData.error) {
        errorMessage = errorData.error;
      }
    } catch {
      // Ignore JSON parse errors, use status text
    }
    throw new Error(errorMessage);
  }

  // Handle empty responses (like DELETE)
  if (response.status === 204) {
    return undefined as unknown as T;
  }

  return response.json();
};

// Transaction API client
export const transactionApi = {
  listAsync: async (budgetId?: number): Promise<Transaction[]> => {
    const params = budgetId ? `?budgetId=${budgetId}` : '';
    const response = await fetch(apiUrl(`/api/transactions${params}`));
    return jsonAsync<Transaction[]>(response);
  },

  getAsync: async (id: number): Promise<Transaction> => {
    const response = await fetch(apiUrl(`/api/transactions/${id}`));
    return jsonAsync<Transaction>(response);
  },

  createAsync: async (transaction: NewTransaction): Promise<Transaction> => {
    const response = await fetch(apiUrl('/api/transactions'), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(transaction),
    });
    return jsonAsync<Transaction>(response);
  },

  updateAsync: async (id: number, updates: UpdateTransaction): Promise<Transaction> => {
    const response = await fetch(apiUrl(`/api/transactions/${id}`), {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates),
    });
    return jsonAsync<Transaction>(response);
  },

  deleteAsync: async (id: number): Promise<void> => {
    const response = await fetch(apiUrl(`/api/transactions/${id}`), {
      method: 'DELETE',
    });
    await jsonAsync(response);
  },
};

// SSE (Server-Sent Events) client
export const createEventSource = (onMessage: (data: unknown) => void): EventSource => {
  const apiUrl = getApiUrl();
  const eventSource = new EventSource(`${apiUrl}/api/events`);

  eventSource.onmessage = (event) => {
    try {
      const data = JSON.parse(event.data);
      onMessage(data);
    } catch (error) {
      console.error('Failed to parse SSE message:', error);
    }
  };

  eventSource.onerror = (error) => {
    console.error('SSE connection error:', error);
  };

  return eventSource;
};