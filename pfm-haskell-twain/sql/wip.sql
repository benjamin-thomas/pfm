/*

litecli> source sql/wip.sql
sqlite> .read sql/wip.sql

 */

SELECT t.transaction_id
     , a.name AS from_account
     , b.name AS to_account
     , t.date
     , datetime(t.date, 'unixepoch') AS date_utc
     , datetime(t.date, 'unixepoch', 'localtime') AS date_tz
     , t.descr
     , t.amount
FROM transactions AS t
INNER JOIN accounts AS a ON t.from_account_id = a.account_id
INNER JOIN accounts AS b ON t.to_account_id = b.account_id;