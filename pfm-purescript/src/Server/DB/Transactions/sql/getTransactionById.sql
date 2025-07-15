SELECT transaction_id
     , budget_id
     , from_account_id
     , to_account_id
     , unique_fit_id
     , date
     , descr_orig
     , descr
     , cents
     , created_at
     , updated_at
FROM transactions
WHERE transaction_id = ?