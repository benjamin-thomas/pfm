import './App.css';
import './components/Buttons.css';
import { BalanceCards } from './components/BalanceCards';
import TransactionList from './components/TransactionList';
import TransactionFilters from './components/TransactionFilters';
import { useAppStateSelector, useAppDispatch } from './hooks';
import { themeActions } from './redux/themeUpdate';
import * as financialEffects from './redux/financialEffects';

type AppProps = {
  apiUrl: string;
}

// Pure component - no side effects, just renders based on Redux state
const App = ({ apiUrl }: AppProps) => {
  const dispatch = useAppDispatch();
  const themeState = useAppStateSelector((state) => state.theme);
  const sseState = useAppStateSelector((state) => state.sse);
  const financialState = useAppStateSelector((state) => state.financial);

  return (
    <div className="container">
      <h1>PFM - TypeScript (Node+React)</h1>

      {/* SSE Connection Status */}
      <pre>SSE: {sseState.lastMessage ? JSON.stringify(sseState.lastMessage) : 'No messages yet'}</pre>

      {/* Dark Mode Toggle */}
      <button
        className="theme-toggle"
        onClick={() => dispatch(themeActions.clickedThemeToggle)}
        title={themeState.isDarkMode ? "Switch to light mode" : "Switch to dark mode"}
      >
        {themeState.isDarkMode ? "🌙" : "☀️"}
      </button>

      {/* Debug Test Buttons */}
      <div style={{ marginLeft: '10px', display: 'flex', gap: '5px', flexWrap: 'wrap' }}>
        <button
          style={{ fontSize: '12px', padding: '4px 8px' }}
          onClick={() => financialEffects.fetchData(apiUrl, dispatch)}
        >
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

      {(() => {
        switch (financialState.data.kind) {
          case 'loading':
            return (
              <div className="section">
                <div>Loading financial data...</div>
              </div>
            );

          case 'error':
            return (
              <div className="section">
                <div>Error loading financial data: {financialState.data.error}</div>
              </div>
            );

          case 'loaded':
            const balances = financialState.data.balances;
            const transactions = financialState.data.transactions;

            return (
              <>
                <div className="section">
                  <BalanceCards balances={balances} />
                </div>

                <div className="section">
                  <div className="transaction-list">
                    <div className="transaction-list__header">

                      <div className="transaction-list__header-title">
                        <h3>Transactions</h3>
                        <span className="transaction-count">
                          {transactions.length} transactions
                        </span>
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
                    <TransactionList transactions={transactions} />
                  </div>
                </div>
              </>
            );

          default:
            const _exhaustive: never = financialState.data;
            throw new Error("Impossible");
        }
      })()}

    </div>
  );
};

export default App;
