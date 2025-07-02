SELECT SOUNDEX(t.descr_orig) AS soundex_descr
     , GROUP_CONCAT(DISTINCT b.account_id) AS suggested_account_ids
     , JSON_GROUP_ARRAY(DISTINCT JSON_OBJECT('id', b.account_id, 'name', b.name)) AS suggested_account_ids
FROM transactions AS t

INNER JOIN accounts AS a
        ON a.account_id = t.from_account_id

INNER JOIN accounts AS b
        ON b.account_id = t.to_account_id

WHERE SOUNDEX(descr) = soundex_descr
AND a.name == 'Checking account'
AND b.name <> 'Unknown_EXPENSE'

GROUP BY soundex_descr
