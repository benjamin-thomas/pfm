import './TransactionFilters.css';

const TransactionFilters = (): React.JSX.Element => {
  return (
    <div className="transaction-search">
      <div className="transaction-search__row">
        <div className="transaction-search__field">
          <label>Description</label>
          <input
            type="text"
            className="transaction-search__input"
            placeholder="Search by description"
          />
        </div>

        <div className="transaction-search__field">
          <label>Min Amount</label>
          <input
            type="number"
            className="transaction-search__input"
            defaultValue="40.00"
          />
        </div>

        <div className="transaction-search__field">
          <label>Max Amount</label>
          <input
            type="number"
            className="transaction-search__input"
            defaultValue="100.00"
          />
        </div>

        <div className="transaction-search__field--button">
          <button className="search-clear-button">
            Clear
          </button>
        </div>
      </div>

      <div className="transaction-search__row transaction-search__row--bottom">
        <div className="transaction-search__field">
          <label>
            <input type="checkbox" defaultChecked />
            {' '}Unknown expenses
          </label>
        </div>
      </div>
    </div>
  );
};

export default TransactionFilters;