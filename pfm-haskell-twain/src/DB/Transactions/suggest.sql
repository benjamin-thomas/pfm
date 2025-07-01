SELECT transaction_id
     , suggestions.suggested_accounts AS suggested_to_accounts
  FROM transactions AS t

INNER JOIN (
    SELECT x.soundex_descr
         , JSON_GROUP_ARRAY
               ( JSON_OBJECT
                   ( 'id', a.account_id
                   , 'name', a.name
                   )
               ) AS suggested_accounts
    FROM (
         SELECT t.to_account_id AS suggested_account_id
              , COUNT(*) AS occurrences
              , SOUNDEX(t.descr_orig) AS soundex_descr
         FROM transactions AS t

         WHERE SOUNDEX(descr_orig) = soundex_descr
           AND t.from_account_id  =  ? -- typically  2 (Checking account)
           AND t.to_account_id   <>  ? -- typically 10 (Unknown_EXPENSE)

         GROUP BY t.to_account_id
         ORDER BY occurrences DESC
    )x

    INNER JOIN accounts AS a
            ON a.account_id = x.suggested_account_id
) AS suggestions
  ON suggestions.soundex_descr = SOUNDEX(t.descr_orig)

WHERE unique_fit_id IS NOT NULL
  AND t.from_account_id =  ? -- typically  2 (Checking account)
  AND t.to_account_id   =  ? -- typically 10 (Unknown_EXPENSE)
