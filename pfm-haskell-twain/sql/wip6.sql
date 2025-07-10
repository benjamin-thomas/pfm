SELECT budget_id
     , DATETIME(b.starts_on, 'unixepoch')  AS starts_on_utc
     , DATETIME(b.ends_on, 'unixepoch')    AS ends_on_utc
     , DATETIME(b.created_at, 'unixepoch') AS created_at_utc
     , DATETIME(b.updated_at, 'unixepoch') AS updated_at_utc
     , JSON_GROUP_ARRAY
           ( JSON_OBJECT
               ( 'budgetLineId'  , l.budget_line_id
               , 'budgetId'      , l.budget_id
               , 'accountId'     , l.account_id
               , 'cents'         , l.cents
               , 'created_at_utc', DATETIME(l.created_at, 'unixepoch')
               , 'updated_at_utc', DATETIME(l.updated_at, 'unixepoch')
               )
           ) FILTER (WHERE l.budget_line_id IS NOT NULL) AS lines_json
FROM budgets AS b
LEFT JOIN budget_lines AS l USING (budget_id)
WHERE b.budget_id = 1