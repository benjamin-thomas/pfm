SELECT x.*
     , SUM(x.flow) OVER (ORDER BY x.transaction_id) AS running_balance
FROM (
    SELECT t.transaction_id
         , a.name AS from_account
         , b.name AS to_account
         , t.date
         , t.descr
         , t.cents * CASE WHEN t.from_account_id = 2 THEN -1 ELSE 1 END AS flow
    FROM transactions AS t

    INNER JOIN accounts AS a
            ON t.from_account_id = a.account_id

    INNER JOIN accounts AS b
            ON t.to_account_id = b.account_id

    WHERE t.to_account_id = 2 OR t.from_account_id = 2
)x

ORDER BY x.transaction_id
;