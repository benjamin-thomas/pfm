SELECT budget_id
     , starts_on
     , datetime(starts_on, 'unixepoch') AS starts_on_utc
     , datetime(starts_on, 'unixepoch', 'localtime') AS starts_on_tz
     , ends_on
     , datetime(ends_on, 'unixepoch') AS ends_on_utc
     , datetime(ends_on, 'unixepoch', 'localtime') AS ends_on_tz
     FROM budgets