// Using legacy_createStore because I hate RTK!
import { legacy_createStore as createStore, combineReducers, compose } from 'redux';
import type { Dispatch } from 'redux';
import { financialReducer } from './redux/financialUpdate';
import { themeReducer } from './redux/themeUpdate';
import { sseReducer } from './redux/sseUpdate';
import type { FinancialState, FinancialAction } from './redux/financialUpdate';
import type { ThemeState, ThemeAction } from './redux/themeUpdate';
import type { SSEState, SSEAction } from './redux/sseUpdate';
import * as themeEffects from './redux/themeEffects';

// Combine all reducers
const rootReducer = combineReducers({
  financial: financialReducer,
  theme: themeReducer,
  sse: sseReducer,
});

// Enable Redux DevTools in development
const composeEnhancers =
  (typeof window !== 'undefined' &&
    window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__) ||
  compose;

// Create store - no middleware needed!
export const store = createStore(
  rootReducer,        // Our combined reducers
  undefined,          // No preloaded state - let reducers use their initial state
  composeEnhancers()  // Redux DevTools enhancer
);

// Handle theme changes automatically when state changes
// This is where side effects happen outside of components
store.subscribe(() => {
  const appState = store.getState();
  themeEffects.applyTheme(appState.theme.isDarkMode);
});

// The root Redux state shape - explicit structure
export type AppState = {
  financial: FinancialState;
  theme: ThemeState;
  sse: SSEState;
};

// Simple dispatch type - no thunk nonsense
type AllActions = FinancialAction | ThemeAction | SSEAction;
export type AppDispatch = Dispatch<AllActions>;

// TypeScript helper for DevTools
declare global {
  interface Window {
    __REDUX_DEVTOOLS_EXTENSION_COMPOSE__?: typeof compose;
  }
}