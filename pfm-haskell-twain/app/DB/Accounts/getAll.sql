SELECT a.account_id
     , a.category_id
     , c.name AS category_name
     , a.name AS account_name
  FROM accounts AS a
INNER JOIN categories AS c USING (category_id)