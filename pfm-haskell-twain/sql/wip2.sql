SELECT x.suggested_account_id
     , a.name AS suggested_account_name
     , x.count
     , SOUNDEX('X4321 CARREFOUR EXPRESS TR 07/08') AS soundex
FROM (
     SELECT b.account_id AS suggested_account_id
          , COUNT(*) AS count
     FROM transactions AS t

     INNER JOIN accounts AS a
             ON a.account_id = t.from_account_id

     INNER JOIN accounts AS b
             ON b.account_id = t.to_account_id

     WHERE SOUNDEX(descr) = SOUNDEX('X4321 CARREFOUR EXPRESS TR 07/08')
       AND a.name == 'Checking account'
       AND b.name <> 'Unknown_EXPENSE'

     GROUP BY suggested_account_id
)x

INNER JOIN accounts AS a
        ON a.account_id = x.suggested_account_id
  ORDER BY x.count DESC;