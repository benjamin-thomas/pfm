WITH inputs(unix_ts) AS (
    SELECT ?
)

INSERT INTO
    budgets ( starts_on
            , ends_on
            )
SELECT strftime('%s', datetime(inputs.unix_ts, 'unixepoch', 'start of month'))
     , strftime('%s', datetime(inputs.unix_ts, 'unixepoch', 'start of month', '+1 month', '-1 second'))
FROM inputs

RETURNING budget_id