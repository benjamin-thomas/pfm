SELECT user_id
     , first_name
     , last_name
     , email
     , created_at
     , updated_at
  FROM users
 WHERE created_at > strftime('%s', '2025-06-01')
