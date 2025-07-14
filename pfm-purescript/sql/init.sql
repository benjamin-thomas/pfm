/*

$ cd ./pfm-haskell-twain
$ litecli ./db.sqlite3
> .read sql/init.sql

=== DATE/TIME HANDLING ===


sqlite> SELECT datetime(1749276636, 'unixepoch');
2025-06-07 06:10:36
sqlite> SELECT datetime(1749276636, 'unixepoch', 'localtime');
2025-06-07 08:10:36


sqlite> SELECT strftime('%s', current_timestamp);
1749276991
sqlite> SELECT datetime(strftime('%s', current_timestamp), 'unixepoch');
2025-06-07 06:17:18
sqlite> SELECT datetime(strftime('%s', current_timestamp), 'unixepoch', 'localtime');
2025-06-07 08:17:25

sqlite> SELECT date(current_date, '+1 day');
2025-06-08
sqlite> SELECT strftime('%s', date(current_date, '+1 day'));
1749340800
sqlite> SELECT strftime('%s', date(current_date, '+2 days'));
1749427200


=== MONEY HANDLING ===

sqlite's type system is underpowered, so I must store cents

sqlite> SELECT CAST(1.234 AS DECIMAL(10,2));
SELECT CAST(1.234 AS DECIMAL(10,2));
1.234

*/

BEGIN TRANSACTION;

DROP TABLE IF EXISTS budget_lines;
DROP TABLE IF EXISTS budgets;
DROP TABLE IF EXISTS transactions;
DROP TABLE IF EXISTS accounts;
DROP TABLE IF EXISTS categories;


-- In SQLite, a column with type INTEGER PRIMARY KEY is an alias for the ROWID (it auto-increments)
CREATE TABLE categories
    ( category_id INTEGER PRIMARY KEY
    , name        TEXT    NOT NULL UNIQUE CHECK (TRIM(name) <> '')
    , created_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    )
    ;

CREATE TRIGGER update_categories_updated_at
AFTER UPDATE ON categories
FOR EACH ROW
BEGIN
    UPDATE categories
    SET updated_at = strftime('%s', current_timestamp)
    WHERE category_id = NEW.category_id;
END;

INSERT INTO categories (name)
VALUES ('Equity')
     , ('Assets')
     , ('Income')
     , ('Expenses')
     ;

CREATE TABLE accounts
    ( account_id  INTEGER PRIMARY KEY
    , category_id INTEGER NOT NULL REFERENCES categories(category_id)
    , name        TEXT    NOT NULL UNIQUE CHECK (TRIM(name) <> '')
    , created_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at  INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    )
    ;

CREATE TRIGGER update_accounts_updated_at
AFTER UPDATE ON accounts
FOR EACH ROW
BEGIN
    UPDATE accounts
    SET updated_at = strftime('%s', current_timestamp)
    WHERE account_id = NEW.account_id;
END;

INSERT INTO accounts (category_id, name)
VALUES (1, 'OpeningBalance')    -- account_id =  1
     , (2, 'Checking account')  -- account_id =  2
     , (2, 'Savings account')   -- account_id =  3
     , (3, 'Unknown_INCOME')    -- account_id =  4
     , (3, 'Employer')          -- account_id =  5
     , (4, 'Unknown_EXPENSE')   -- account_id =  6
     , (4, 'Groceries')         -- account_id =  7
     , (4, 'Communications')    -- account_id =  8
     , (4, 'Transport')         -- account_id =  9
     , (4, 'Health')            -- account_id = 10
     , (4, 'Energy')            -- account_id = 11
     , (4, 'Clothing')          -- account_id = 12
     , (4, 'Leisure')           -- account_id = 13
     ;

CREATE TABLE transactions
    ( transaction_id  INTEGER        PRIMARY KEY
    , budget_id       INTEGER        NOT NULL REFERENCES budgets(budget_id)
    , from_account_id INTEGER        NOT NULL REFERENCES accounts(account_id)
    , to_account_id   INTEGER        NOT NULL REFERENCES accounts(account_id)
    , unique_fit_id   TEXT           NULL
    , date            INTEGER        NOT NULL
    , descr_orig      TEXT           NOT NULL -- will enable classifying if source=OFX
    , descr           TEXT           NOT NULL
    , cents           INTEGER        NOT NULL CHECK (cents > 0)
    , created_at      INTEGER        NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at      INTEGER        NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , CHECK (from_account_id <> to_account_id)
    )
    ;

CREATE INDEX idx_transactions_from_account_id ON transactions(from_account_id);
CREATE INDEX idx_transactions_to_account_id   ON transactions(to_account_id);
CREATE INDEX idx_transactions_date            ON transactions(date);

CREATE TRIGGER  update_transactions_updated_at
AFTER UPDATE ON transactions
FOR EACH ROW
BEGIN
    UPDATE transactions
       SET updated_at = strftime('%s', current_timestamp)
     WHERE transaction_id = NEW.transaction_id;
END;

CREATE TABLE budgets
    ( budget_id  INTEGER PRIMARY KEY
    , starts_on INTEGER NOT NULL
    , ends_on   INTEGER NOT NULL
    , created_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , CHECK (starts_on >= 0 AND starts_on < ends_on)
    )
    ;

-- Create a unique index to help with the ON CONFLICT clause
CREATE UNIQUE INDEX idx_budgets_no_overlap ON budgets(starts_on, ends_on);

-- Trigger to prevent overlapping budget periods
CREATE TRIGGER prevent_budget_overlap
BEFORE INSERT ON budgets
FOR EACH ROW
BEGIN
    SELECT RAISE(ABORT, 'Budget period already defined or overlaps' )
     WHERE EXISTS (
         SELECT 1 FROM budgets
          WHERE NEW.starts_on <= ends_on
            AND NEW.ends_on   >= starts_on
    );
END;

-- Similar trigger for updates
CREATE TRIGGER prevent_budget_overlap_update
BEFORE UPDATE ON budgets
FOR EACH ROW
BEGIN
    SELECT RAISE(ABORT, 'Budget period already defined or overlaps' )
     WHERE EXISTS (
         SELECT 1 FROM budgets
          WHERE NEW.starts_on <= ends_on
            AND NEW.ends_on   >= starts_on
    );
END;

CREATE TRIGGER update_budgets_updated_at
AFTER UPDATE ON budgets
FOR EACH ROW
BEGIN
    UPDATE budgets
       SET updated_at = strftime('%s', current_timestamp)
     WHERE budget_id  = NEW.budget_id;
END;

CREATE TABLE budget_lines
    ( budget_line_id INTEGER PRIMARY KEY
    , budget_id      INTEGER NOT NULL REFERENCES budgets(budget_id)
    , account_id     INTEGER NOT NULL REFERENCES accounts(account_id)
    , cents          INTEGER NOT NULL
    , created_at     INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at     INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    )
    ;

CREATE TRIGGER update_budget_lines_updated_at
AFTER UPDATE ON budget_lines
FOR EACH ROW
BEGIN
    UPDATE budget_lines
       SET updated_at = strftime('%s', current_timestamp)
     WHERE budget_line_id = NEW.budget_line_id;
END;



COMMIT;
