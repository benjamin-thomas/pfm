/*

$ cd ./pfm-haskell-twain
$ litecli ./db.sqlite3
> .read sql/init.sql

 */

DROP TABLE IF EXISTS transactions;
DROP TABLE IF EXISTS accounts;
DROP TABLE IF EXISTS categories;

-- In SQLite, a column with type INTEGER PRIMARY KEY is an alias for the ROWID (it auto-increments)
CREATE TABLE categories
    ( category_id INTEGER PRIMARY KEY
    , name        TEXT    NOT NULL UNIQUE CHECK (TRIM(name) <> '')
    )
    ;

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
    )
    ;

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
    ( transaction_id  INTEGER PRIMARY KEY
    , from_account_id INTEGER NOT NULL REFERENCES accounts(account_id)
    , to_account_id   INTEGER NOT NULL REFERENCES accounts(account_id)
    , date            INTEGER NOT NULL
    , descr           TEXT    NOT NULL
    , amount          DECIMAL NOT NULL CHECK (amount > 0)
    )
    ;

INSERT INTO transactions (from_account_id, to_account_id, date, descr, amount)
VALUES (1, 2, (SELECT strftime('%s', date(current_date, '+0 days'))), 'Opening balance', 1000.00) -- Opening balance to Checking account
     , (2, 6, (SELECT strftime('%s', date(current_date, '+1 days'))), 'Groceries',          9.99) -- Checking account to Spar
     , (2, 8, (SELECT strftime('%s', date(current_date, '+2 days'))), 'Book purchase',     54.99) -- Checking account to Amazon
     , (2, 6, (SELECT strftime('%s', date(current_date, '+3 days'))), 'Groceries, again',  37.42) -- Checking account to Spar
     , (4, 2, (SELECT strftime('%s', date(current_date, '+4 days'))), 'Salary',            12.99) -- EmployerABC to Checking account
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

*/
