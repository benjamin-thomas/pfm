SELECT budget_id
     , DATETIME(starts_on, 'unixepoch')  AS starts_on_utc
     , DATETIME(ends_on, 'unixepoch')    AS ends_on_utc
     , DATETIME(created_at, 'unixepoch') AS created_at_utc
     , DATETIME(updated_at, 'unixepoch') AS updated_at_utc
 FROM budgets
WHERE budget_id = ?