import { createTransactionApi } from '../api/client';
import type { AccountBalanceRead } from '@shared/types';
import type { AppDispatch } from '../store';
import { financialActions } from './financialUpdate';

// Plain async function - no thunk magic!
export const fetchData = async (apiUrl: string, appDispatch: AppDispatch): Promise<void> => {
  // Start loading
  appDispatch(financialActions.loading);

  try {
    // Fetch both balances and transactions in parallel
    const [balancesResponse, transactions] = await Promise.all([
      fetch(`${apiUrl}/api/balances`),
      createTransactionApi(apiUrl).listAsync(1) // budgetId = 1
    ]);

    // Handle balances response
    if (!balancesResponse.ok) {
      appDispatch(financialActions.failed(`HTTP error! status: ${balancesResponse.status}`));
      return;
    }
    const balances: AccountBalanceRead[] = await balancesResponse.json();

    appDispatch(financialActions.loaded({ balances, transactions }));
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    appDispatch(financialActions.failed(errorMessage));
  }
};