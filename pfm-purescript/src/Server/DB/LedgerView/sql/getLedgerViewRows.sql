SELECT y.transaction_id
     , y.budget_id
     , y.from_account_id
     , y.from_account_name
     , y.to_account_id
     , y.to_account_name
     , y.date AS date_unix
     , date(y.date, 'unixepoch') AS date
     , y.descr
     , SOUNDEX(y.descr) AS soundex_descr
     , y.flow_cents
     , printf('%.2f', y.flow_cents / 100.0) AS flow
     , y.running_balance_cents
     , printf('%.2f', y.running_balance_cents / 100.0) AS running_balance
     , y.created_at AS created_at_unix
     , datetime(y.created_at, 'unixepoch') AS created_at_utc
     , datetime(y.created_at, 'unixepoch', 'localtime') AS created_at_tz
     , y.updated_at AS updated_at_unix
     , datetime(y.updated_at, 'unixepoch') AS updated_at_utc
     , datetime(y.updated_at, 'unixepoch', 'localtime') AS updated_at_tz
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
                , t.cents * CASE WHEN t.from_account_id = $accountId THEN -1 ELSE 1 END AS flow_cents
        FROM transactions AS t

        INNER JOIN accounts AS a
                ON t.from_account_id = a.account_id

        INNER JOIN accounts AS b
                ON t.to_account_id = b.account_id

        INNER JOIN budgets AS bu
                ON t.budget_id = bu.budget_id

        WHERE (t.to_account_id = $accountId OR t.from_account_id = $accountId)
          AND (CASE WHEN $descriptionFilter IS NOT NULL AND TRIM($descriptionFilter) <> "" THEN t.descr LIKE '%' || $descriptionFilter || '%' ELSE 1 END)
          AND (CASE WHEN $minAmountCents IS NOT NULL THEN ABS(t.cents) >= $minAmountCents ELSE 1 END)
          AND (CASE WHEN $maxAmountCents IS NOT NULL THEN ABS(t.cents) <= $maxAmountCents ELSE 1 END)
          AND (CASE WHEN $unknownExpensesOnly = 1 THEN t.to_account_id = 6 ELSE 1 END)
        )x
)y

-- NOTE: The front computes the prior balance (first row is 0)
--       So, because of this, it's better to reverse the rows there for now.
ORDER BY y.date ASC, y.transaction_id ASC