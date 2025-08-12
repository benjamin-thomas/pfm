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

=== PRIMARY KEYS ===

In SQLite, a column with type INTEGER PRIMARY KEY is an alias for the ROWID (it auto-increments)

*/

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