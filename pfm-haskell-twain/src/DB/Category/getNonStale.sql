SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
 WHERE updated_at > (strftime('%s', 'now') - 90 * 24 * 60 * 60)
