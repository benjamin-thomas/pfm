SELECT y.transaction_id
     , y.budget_id
     , y.from_account_id
     , y.from_account_name
     , y.to_account_id
     , y.to_account_name
     , y.date AS date_unix
     , date(y.date, 'unixepoch') AS date
     , y.descr
     , y.flow_cents
     , y.running_balance_cents
     , y.created_at AS created_at_unix
     , y.updated_at AS updated_at_unix
FROM (
        SELECT x.*
      , SUM(x.flow_cents) OVER (ORDER BY x.date ASC, x.transaction_id ASC) AS running_balance_cents
        FROM (
        SELECT t.transaction_id
                , t.budget_id
                , a.name AS from_account_name
                , a.account_id AS from_account_id
                , b.name AS to_account_name
                , b.account_id AS to_account_id
                , t.date
                , t.descr
                , t.created_at
                , t.updated_at
                , t.cents * CASE WHEN t.from_account_id = ? THEN -1 ELSE 1 END AS flow_cents
        FROM transactions AS t

        INNER JOIN accounts AS a
                ON t.from_account_id = a.account_id

        INNER JOIN accounts AS b
                ON t.to_account_id = b.account_id

        INNER JOIN budgets AS bu
                ON t.budget_id = bu.budget_id

        WHERE t.to_account_id = ? OR t.from_account_id = ?
        )x
)y
ORDER BY y.date ASC, y.transaction_id ASC