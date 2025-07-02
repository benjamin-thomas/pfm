-- Use this query!

SELECT soundex_descr
     , JSON_GROUP_ARRAY(
           JSON_OBJECT
               ( 'id', account_id
               , 'name', name
               , 'occurrences', occurrence_count
               )
       ) AS suggested_accounts
FROM (
    SELECT SOUNDEX(t.descr_orig) AS soundex_descr
         , b.account_id
         , b.name
         , COUNT(*) AS occurrence_count
    FROM transactions AS t

    INNER JOIN accounts AS a
            ON a.account_id = t.from_account_id

    INNER JOIN accounts AS b
            ON b.account_id = t.to_account_id

    WHERE SOUNDEX(descr) = SOUNDEX(t.descr_orig)
      AND a.name == 'Checking account'
      AND b.name <> 'Unknown_EXPENSE'

    GROUP BY soundex_descr, b.account_id
    ORDER BY COUNT(*) DESC
) AS account_counts
GROUP BY soundex_descr