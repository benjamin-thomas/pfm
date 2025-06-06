/*

$ cd ./pfm-haskell-twain
$ litecli ./test.db
> .read sql/init.sql

 */

-- In SQLite, a column with type INTEGER PRIMARY KEY is an alias for the ROWID
CREATE TABLE category
    ( category_id INTEGER PRIMARY KEY
    , name TEXT NOT NULL UNIQUE CHECK (TRIM(name) <> '')
    )
    ;

INSERT INTO category (name) VALUES ('Assets');
INSERT INTO category (name) VALUES ('Expenses');
INSERT INTO category (name) VALUES ('Income');
INSERT INTO category (name) VALUES ('Equity');
