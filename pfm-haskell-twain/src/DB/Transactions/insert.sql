INSERT INTO transactions
    ( from_account_id
    , to_account_id
    , source_id
    , date
    , descr_orig
    , descr
    , cents
    )
VALUES
    ( ?
    , ?
    , (SELECT source_id FROM sources WHERE name = ?)
    , ?
    , ?
    , ?
    , ?
    )
    ;