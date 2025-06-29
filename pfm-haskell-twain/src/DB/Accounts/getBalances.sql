SELECT a.account_id
     , a.category_id
     , c.name AS category_name
     , a.name AS account_name
     , COALESCE(x.added, 0) - COALESCE(x.removed, 0) AS account_balance
FROM accounts AS a
INNER JOIN categories AS c ON a.category_id = c.category_id
LEFT JOIN (
    SELECT account_id
         , SUM(outgoing) AS removed
         , SUM(incoming) AS added
    FROM (
        SELECT from_account_id AS account_id
             , cents AS outgoing
             , 0 AS incoming
        FROM transactions

        UNION ALL

        SELECT to_account_id AS account_id
             , 0 AS outgoing
             , cents AS incoming
        FROM transactions
    )
    GROUP BY account_id
)x ON a.account_id = x.account_id
WHERE a.account_id IN (__QUESTIONS__);
