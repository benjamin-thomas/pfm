/*

cat ./src/Server/DB/LedgerView/sql/getLedgerViewRows.sql | accountId=2 descriptionFilter=NULL soundexDescr=NULL minAmountCents=NULL maxAmountCents=NULL filterUnknownExpenses=NULL envsubst | sqlite3 -header -csv ./db.sqlite | csvlook | less -S
cat ./src/Server/DB/LedgerView/sql/getLedgerViewRows.sql | accountId=2 descriptionFilter="'APPLE'" soundexDescr=NULL minAmountCents=NULL maxAmountCents=NULL filterUnknownExpenses=NULL envsubst | sqlite3 -header ./db.sqlite -csv | csvlook | less -S

 */
SELECT z.transaction_id
     , z.budget_id
     , z.from_account_id
     , z.from_account_name
     , z.to_account_id
     , z.to_account_name
     , z.date AS date_unix
     , date(z.date, 'unixepoch') AS date
     , z.descr
     , SOUNDEX(z.descr) AS soundex_descr
     , z.flow_cents
     , printf('%.2f', z.flow_cents / 100.0) AS flow
     , z.running_balance_cents
     , printf('%.2f', z.running_balance_cents / 100.0) AS running_balance
     , z.prior_balance_cents
     , printf('%.2f', z.prior_balance_cents / 100.0) AS prior_balance
     , z.created_at AS created_at_unix
     , datetime(z.created_at, 'unixepoch') AS created_at_utc
     , datetime(z.created_at, 'unixepoch', 'localtime') AS created_at_tz
     , z.updated_at AS updated_at_unix
     , datetime(z.updated_at, 'unixepoch') AS updated_at_utc
     , datetime(z.updated_at, 'unixepoch', 'localtime') AS updated_at_tz
FROM (
        SELECT y.*
      , COALESCE(LAG(y.running_balance_cents) OVER (ORDER BY y.date ASC, y.transaction_id ASC), 0) AS prior_balance_cents
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
        )x
      )y
   )z

WHERE (CASE WHEN $descriptionFilter IS NOT NULL AND TRIM($descriptionFilter) <> "" THEN z.descr LIKE '%' || $descriptionFilter || '%' ELSE 1 END)
  AND (CASE WHEN $soundexDescr IS NOT NULL AND TRIM($soundexDescr) <> "" THEN SOUNDEX(z.descr) = $soundexDescr ELSE 1 END)
  AND (CASE WHEN $minAmountCents IS NOT NULL THEN ABS(z.flow_cents) >= $minAmountCents ELSE 1 END)
  AND (CASE WHEN $maxAmountCents IS NOT NULL THEN ABS(z.flow_cents) <= $maxAmountCents ELSE 1 END)
  AND (CASE WHEN $filterUnknownExpenses = 1 THEN z.to_account_id = 6 ELSE 1 END)

ORDER BY z.date DESC, z.transaction_id DESC