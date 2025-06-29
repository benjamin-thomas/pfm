UPDATE transactions
SET from_account_id  = ?
  , to_account_id    = ?
  , date             = ?
  , descr            = ?
  , cents            = ?
WHERE transaction_id = ?