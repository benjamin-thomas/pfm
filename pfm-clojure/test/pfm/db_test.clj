(ns pfm.db-test
  (:require [clojure.test :refer :all]
            [pfm.db :as db]
            [clojure.java.io :as io]))


;; *earmuffs* = convention for dynamic vars (can be rebound in different threads)
;; ^:dynamic = metadata telling Clojure this var can be rebound
(def ^:dynamic *test-db-file* nil)

(defn setup-test-db []
  ;; Create unique filename using timestamp + random number
  (let [unique-id (str (System/currentTimeMillis) "-" (rand-int 10000))
        db-file (str "db.test." unique-id ".sqlite")]
    ;; alter-var-root = permanently change the root value of a var
    ;; constantly = function that always returns the same value (like: fn [_] db-file)
    (alter-var-root #'*test-db-file* (constantly db-file))
    db-file))

(defn cleanup-test-db []
  (when *test-db-file*
    (let [db-file (io/file *test-db-file*)]
      (when (.exists db-file)
        (.delete db-file)))))

(use-fixtures :each 
  (fn [test-fn] 
    (let [db-file (setup-test-db)]
      (try 
        ;; with-redefs = temporarily replace functions/vars during test execution
        ;; This makes db/db-spec point to our unique test database
        (with-redefs [db/db-spec {:dbtype "sqlite" :dbname db-file}]
          (test-fn))
        (finally 
          (cleanup-test-db))))))

(deftest get-connection-test
  (testing "can establish database connection"
    (let [conn (db/get-connection)]
      (is (some? conn)))))

(deftest query-test
  (testing "can execute simple query via database"
    (let [result (db/query "SELECT 1 + 2 as result")]
      (is (= 3 (-> result first :result))))))

(deftest create-transactions-table-test
  (testing "can create transactions table"
    (db/create-transactions-table!)
    (is true)))

