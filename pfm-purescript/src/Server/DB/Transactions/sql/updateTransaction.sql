UPDATE transactions
   SET from_account_id = ?
     , to_account_id = ?
     , date = ?
     , descr = ?
     , cents = ?
     , updated_at = strftime('%s', 'now')
WHERE transaction_id = ?