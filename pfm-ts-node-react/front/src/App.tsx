import { useState, useEffect } from 'react';
import './App.css';
import './components/Buttons.css';
import { BalanceCards } from './components/BalanceCards';
import TransactionList from './components/TransactionList';
import TransactionFilters from './components/TransactionFilters';
import type { AccountBalanceRead } from '@shared/types';

type Status<T> =
  | { kind: 'loading' }
  | { kind: 'loaded'; data: T }
  | { kind: 'error'; error: string };

function App() {
  const [isDarkMode, setIsDarkMode] = useState(true);
  const [balancesStatus, setBalancesStatus] = useState<Status<AccountBalanceRead[]>>({ kind: 'loading' });

  // Apply theme on mount and when isDarkMode changes
  useEffect(() => {
    if (isDarkMode) {
      document.documentElement.classList.add('dark-theme');
    } else {
      document.documentElement.classList.remove('dark-theme');
    }
  }, [isDarkMode]);

  const toggleTheme = (): void => {
    setIsDarkMode(!isDarkMode);
  };

  useEffect(() => {
    const fetchBalances = async (): Promise<void> => {
      try {
        const response = await fetch('http://localhost:8083/api/balances');
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        const balances: AccountBalanceRead[] = await response.json();
        setBalancesStatus({ kind: 'loaded', data: balances });
      } catch (error) {
        setBalancesStatus({
          kind: 'error',
          error: error instanceof Error ? error.message : 'Unknown error'
        });
      }
    };

    fetchBalances();
  }, []);

  return (
    <div className="container">
      <h1>PFM - TypeScript (Node+React)</h1>

      {/* SSE Connection Status */}
      <pre>[BOGUS] Connected: TypeScript SSE established</pre>

      {/* Dark Mode Toggle */}
      <button
        className="theme-toggle"
        onClick={toggleTheme}
        title={isDarkMode ? "Switch to light mode" : "Switch to dark mode"}
      >
        {isDarkMode ? "🌙" : "☀️"}
      </button>

      {/* Debug Test Buttons */}
      <div style={{ marginLeft: '10px', display: 'flex', gap: '5px', flexWrap: 'wrap' }}>
        <button style={{ fontSize: '12px', padding: '4px 8px' }}>
          Test Refresh
        </button>
        <button style={{ fontSize: '12px', padding: '4px 8px' }}>
          Save ✓
        </button>
        <button style={{ fontSize: '12px', padding: '4px 8px' }}>
          Save ✗
        </button>
        <button style={{ fontSize: '12px', padding: '4px 8px' }}>
          Delete ✓
        </button>
        <button style={{ fontSize: '12px', padding: '4px 8px' }}>
          Delete ✗
        </button>
      </div>

      <div className="section">
        {balancesStatus.kind === 'loading' && <div>Loading balances...</div>}
        {balancesStatus.kind === 'error' && <div>Error loading balances: {balancesStatus.error}</div>}
        {balancesStatus.kind === 'loaded' && <BalanceCards balances={balancesStatus.data} />}
      </div>

      <div className="section">
        <div className="transaction-list">
          <div className="transaction-list__header">
            <div className="transaction-list__header-title">
              <h3>Transactions</h3>
              <span className="transaction-count">8 transactions</span>
            </div>
            <div className="transaction-list__header-buttons">
              <button className="button">
                💡 Apply All Suggestions
              </button>
              <button className="button button--primary">
                Add Transaction
              </button>
            </div>
          </div>

          <TransactionFilters />
          <TransactionList budgetId={1} />
        </div>
      </div>
    </div>
  );
}

export default App;
