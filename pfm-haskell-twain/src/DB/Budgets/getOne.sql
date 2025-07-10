SELECT budget_id
     , STRFTIME('%Y-%m-%dT%H:%M:%SZ', b.starts_on, 'unixepoch')  AS starts_on_utc
     , STRFTIME('%Y-%m-%dT%H:%M:%SZ', b.ends_on, 'unixepoch')    AS ends_on_utc
     , STRFTIME('%Y-%m-%dT%H:%M:%SZ', b.created_at, 'unixepoch') AS created_at_utc
     , STRFTIME('%Y-%m-%dT%H:%M:%SZ', b.updated_at, 'unixepoch') AS updated_at_utc
     , JSON_GROUP_ARRAY
           ( JSON_OBJECT
               ( 'budgetLineId'  , l.budget_line_id
               , 'budgetId'      , l.budget_id
               , 'accountId'     , l.account_id
               , 'cents'         , l.cents
               , 'createdAtUtc', STRFTIME('%Y-%m-%dT%H:%M:%SZ', l.created_at, 'unixepoch')
               , 'updatedAtUtc', STRFTIME('%Y-%m-%dT%H:%M:%SZ', l.updated_at, 'unixepoch')
               )
           ) FILTER (WHERE l.budget_line_id IS NOT NULL) AS lines_json
FROM budgets AS b
LEFT JOIN budget_lines AS l USING (budget_id)
WHERE b.budget_id = ?