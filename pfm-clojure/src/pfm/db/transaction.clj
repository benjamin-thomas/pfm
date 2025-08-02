(ns pfm.db.transaction
  (:require [hugsql.core :as hugsql]))

;; Load HugSQL functions from SQL file - this generates all the functions
;; This creates: get-all-transactions, get-transaction-by-id, 
;;               insert-transaction!, update-transaction!, delete-transaction!
(hugsql/def-db-fns "sql/transactions.sql")