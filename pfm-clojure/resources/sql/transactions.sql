-- :name insert-transaction! :! :n
-- :doc Insert a new transaction and return affected row count
INSERT INTO transactions (description, amount, created_at_unix)
VALUES (:description, :amount, :created_at_unix);

-- :name get-all-transactions :? :*
-- :doc Get all transactions ordered by creation time
SELECT id, description, amount, created_at_unix
FROM transactions 
ORDER BY created_at_unix;

-- :name get-transaction-by-id :? :1
-- :doc Get a single transaction by ID
SELECT id, description, amount, created_at_unix
FROM transactions 
WHERE id = :id;

-- :name update-transaction! :! :n
-- :doc Update a transaction by ID
UPDATE transactions 
SET description = :description, 
    amount = :amount,
    created_at_unix = :created_at_unix
WHERE id = :id;

-- :name delete-transaction! :! :n
-- :doc Delete a transaction by ID
DELETE FROM transactions 
WHERE id = :id;