(ns pfm.db-test
  (:require [clojure.test :refer :all]
            [pfm.db :as db]
            [pfm.test-helpers :as helpers]))

;; :once runs before/after ALL tests in namespace
;; :each runs before/after EACH test
(use-fixtures :once helpers/with-test-db-suite)
(use-fixtures :each helpers/with-transaction-rollback)

(deftest get-connection-test
  (testing "can establish database connection"
    (let [conn (db/get-connection)]
      (is (some? conn)))))

(deftest query-test
  (testing "can execute simple query via database"
    (let [result (db/query "SELECT 1 + 2 as result")]
      (is (= 3 (-> result first :result))))))


