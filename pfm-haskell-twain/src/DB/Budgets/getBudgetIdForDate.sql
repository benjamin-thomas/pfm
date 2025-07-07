SELECT budget_id
  FROM budgets
 WHERE starts_on = strftime('%s', DATETIME(?, 'unixepoch', 'start of month'))
