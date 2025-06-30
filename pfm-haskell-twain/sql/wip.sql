SELECT DISTINCT a.name AS src_account
     , b.name AS dest_account
     , descr
     , SOUNDEX(descr)
FROM transactions AS t

INNER JOIN accounts AS a
ON a.account_id = t.from_account_id

INNER JOIN accounts AS b
ON b.account_id = t.to_account_id

WHERE SOUNDEX(descr) = SOUNDEX('X4321 CARREFOUR MARKET NIC 25/06')
AND b.name <> 'Unknown_EXPENSE'

ORDER BY 3;