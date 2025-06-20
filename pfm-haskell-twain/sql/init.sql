/*

$ cd ./pfm-haskell-twain
$ litecli ./db.sqlite3
> .read sql/init.sql

 */

BEGIN TRANSACTION;

DROP TABLE IF EXISTS transactions;
DROP TABLE IF EXISTS accounts;
DROP TABLE IF EXISTS categories;

-- TEMP/BOGUS START
DROP TABLE IF EXISTS users; -- temp bogus table, just for testing stuff..
CREATE TABLE users
    ( user_id   INTEGER PRIMARY KEY
    , first_name TEXT    NOT NULL
    , last_name  TEXT    NOT NULL
    , email     TEXT    NOT NULL
    , created_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , updated_at INTEGER NOT NULL DEFAULT (strftime('%s', current_timestamp))
    , CHECK (TRIM(first_name) <> '')
    , CHECK (TRIM(last_name) <> '')
    , CHECK (TRIM(email) <> '')
    , UNIQUE (email)
    , UNIQUE (first_name, last_name)
    )
    ;

CREATE TRIGGER update_users_updated_at
AFTER UPDATE ON users
FOR EACH ROW
BEGIN
    UPDATE users
    SET updated_at = strftime('%s', current_timestamp)
    WHERE user_id = NEW.user_id;
END;

INSERT INTO users (first_name, last_name, email)
VALUES ('John', 'Doe', 'john@example.com')
     , ('Jane', 'Doe', 'jane@example.com')
     ;
-- TEMP/BOGUS STOP

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
VALUES (1, 'OpeningBalance')      -- account_id = 1
     , (2, 'Checking account')    -- account_id = 2
     , (2, 'Savings account')     -- account_id = 3
     , (3, 'EmployerABC')         -- account_id = 4
     , (3, 'CustomerXYZ')         -- account_id = 5
     , (4, 'Spar')                -- account_id = 6
     , (4, 'Tesco')               -- account_id = 7
     , (4, 'Amazon')              -- account_id = 8
     ;

/*
SELECT * FROM account INNER JOIN category USING (category_id);
*/

CREATE TABLE transactions
    ( transaction_id  INTEGER        PRIMARY KEY
    , from_account_id INTEGER        NOT NULL REFERENCES accounts(account_id)
    , to_account_id   INTEGER        NOT NULL REFERENCES accounts(account_id)
    , date            INTEGER        NOT NULL
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

INSERT INTO transactions (from_account_id, to_account_id, date, descr, cents)
VALUES (1, 2, (SELECT strftime('%s', date(current_date, '+0 days'))), 'Opening balance', 100000) -- Opening balance to Checking account
     , (2, 6, (SELECT strftime('%s', date(current_date, '+1 days'))), 'Groceries',          999) -- Checking account to Spar
     , (2, 8, (SELECT strftime('%s', date(current_date, '+2 days'))), 'Book purchase',     5499) -- Checking account to Amazon
     , (2, 6, (SELECT strftime('%s', date(current_date, '+3 days'))), 'Groceries, again',  3742) -- Checking account to Spar
     , (4, 2, (SELECT strftime('%s', date(current_date, '+4 days'))), 'Salary',           10000) -- EmployerABC to Checking account
     ;

/*

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

COMMIT;
