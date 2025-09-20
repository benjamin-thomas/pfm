import type { Transaction, NewTransaction, UpdateTransaction } from '@shared/types';

// Helper to build full API URL
const apiUrl = (apiBaseUrl: string, path: string): string => `${apiBaseUrl}${path}`;

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

// Transaction API client factory
export const createTransactionApi = (apiBaseUrl: string) => ({
  listAsync: async (budgetId?: number): Promise<Transaction[]> => {
    const params = budgetId ? `?budgetId=${budgetId}` : '';
    const response = await fetch(apiUrl(apiBaseUrl, `/api/transactions${params}`));
    return jsonAsync<Transaction[]>(response);
  },

  getAsync: async (id: number): Promise<Transaction> => {
    const response = await fetch(apiUrl(apiBaseUrl, `/api/transactions/${id}`));
    return jsonAsync<Transaction>(response);
  },

  createAsync: async (transaction: NewTransaction): Promise<Transaction> => {
    const response = await fetch(apiUrl(apiBaseUrl, '/api/transactions'), {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(transaction),
    });
    return jsonAsync<Transaction>(response);
  },

  updateAsync: async (id: number, updates: UpdateTransaction): Promise<Transaction> => {
    const response = await fetch(apiUrl(apiBaseUrl, `/api/transactions/${id}`), {
      method: 'PUT',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(updates),
    });
    return jsonAsync<Transaction>(response);
  },

  deleteAsync: async (id: number): Promise<void> => {
    const response = await fetch(apiUrl(apiBaseUrl, `/api/transactions/${id}`), {
      method: 'DELETE',
    });
    await jsonAsync(response);
  },
});

// SSE (Server-Sent Events) client factory
export const createEventSource = (apiBaseUrl: string, onMessage: (data: unknown) => void): EventSource => {
  const eventSource = new EventSource(`${apiBaseUrl}/api/events`);

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