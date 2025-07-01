SELECT x.soundex_descr
     , JSON_GROUP_ARRAY
           ( JSON_OBJECT
               ( 'id', a.account_id
               , 'name', a.name
               , 'occurrences', x.occurrences
               )
           ) AS suggested_accounts
FROM (
     SELECT b.account_id AS suggested_account_id
          , COUNT(*) AS occurrences
          , SOUNDEX('X4321 CARREFOUR EXPRESS TR 07/08') AS soundex_descr
     FROM transactions AS t

     INNER JOIN accounts AS a
             ON a.account_id = t.from_account_id

     INNER JOIN accounts AS b
             ON b.account_id = t.to_account_id

     WHERE SOUNDEX(descr) = soundex_descr
       AND a.name == 'Checking account'
       AND b.name <> 'Unknown_EXPENSE'

     GROUP BY suggested_account_id
     ORDER BY occurrences DESC
)x

INNER JOIN accounts AS a
        ON a.account_id = x.suggested_account_id