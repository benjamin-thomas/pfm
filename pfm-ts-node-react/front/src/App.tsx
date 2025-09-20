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
function App({ apiUrl }: AppProps) {
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

      <div className="section">
        {financialState.data.kind === 'loading' && <div>Loading financial data...</div>}
        {financialState.data.kind === 'error' && <div>Error loading financial data: {financialState.data.error}</div>}
        {financialState.data.kind === 'loaded' && <BalanceCards balances={financialState.data.data.balances} />}
      </div>

      <div className="section">
        <div className="transaction-list">
          <div className="transaction-list__header">
            <div className="transaction-list__header-title">
              <h3>Transactions</h3>
              <span className="transaction-count">
                {financialState.data.kind === 'loaded' ? `${financialState.data.data.transactions.length} transactions` : '... transactions'}
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
          {financialState.data.kind === 'loading' && <div>Loading transactions...</div>}
          {financialState.data.kind === 'error' && <div>Error loading transactions: {financialState.data.error}</div>}
          {financialState.data.kind === 'loaded' && <TransactionList transactions={financialState.data.data.transactions} />}
        </div>
      </div>
    </div>
  );
}

export default App;
