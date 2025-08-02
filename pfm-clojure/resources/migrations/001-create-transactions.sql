CREATE TABLE transactions
(
    id              INTEGER PRIMARY KEY AUTOINCREMENT,
    description     TEXT    NOT NULL,
    amount          REAL    NOT NULL,
    created_at_unix INTEGER NOT NULL
);