import type { AccountBalanceRead, Transaction } from '@shared/types';

// Shared Status type for async operations - proper ADT
type Status<T> =
  | { kind: 'loading' }
  | { kind: 'loaded' } & T
  | { kind: 'error'; error: string };

// The complete financial data that we load in one go
type FinancialData = {
  balances: AccountBalanceRead[];
  transactions: Transaction[];
};

interface FinancialState {
  data: Status<FinancialData>;
}

const initialState: FinancialState = {
  data: { kind: 'loading' },
};

// Action types - no shouting, we're civilized
type FinancialAction =
  | { type: 'loading' }
  | { type: 'loaded'; payload: FinancialData }
  | { type: 'failed'; reason: string };

// Export actions as a record for clean namespaced API
export const financialActions = {
  loading: { type: 'loading' } as FinancialAction,
  loaded: (payload: FinancialData): FinancialAction => ({ type: 'loaded', payload }),
  failed: (reason: string): FinancialAction => ({ type: 'failed', reason }),
};

// Pure reducer with proper object spreading - no Immer magic
export const financialReducer = (
  state: FinancialState = initialState,
  action: FinancialAction
): FinancialState => {
  switch (action.type) {
    case 'loading':
      return {
        ...state,
        data: { kind: 'loading' }
      };
    case 'loaded':
      return {
        ...state,
        data: { kind: 'loaded', ...action.payload }
      };
    case 'failed':
      return {
        ...state,
        data: { kind: 'error', error: action.reason }
      };
    default:
      const _exhaustive: never = action;
      return state;
  }
};

export type { FinancialState, FinancialAction };