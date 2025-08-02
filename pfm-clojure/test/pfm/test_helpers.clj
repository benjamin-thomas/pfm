(ns pfm.test-helpers
  (:require [pfm.db :as db]
            [clojure.java.io :as io]
            [next.jdbc :as jdbc]))

(def ^:dynamic *test-db-file* nil)
(def ^:dynamic *test-connection* nil)
(def ^:dynamic *test-transaction* nil)

(defn setup-test-db
  "Create test database and run migrations - called once per test suite"
  []
  (let [unique-id (str (System/currentTimeMillis) "-" (rand-int 10000))
        db-file (str "db.test." unique-id ".sqlite")]
    (alter-var-root #'*test-db-file* (constantly db-file))
    ;; Setup database with migrations
    (with-redefs [db/db-spec {:dbtype "sqlite" :dbname db-file}]
      (db/create-transactions-table!))
    db-file))

(defn cleanup-test-db
  "Remove test database - called once after all tests"
  []
  (when *test-db-file*
    (let [db-file (io/file *test-db-file*)]
      (when (.exists db-file)
        (.delete db-file)))))

(defn with-test-db-suite
  "Fixture to setup DB once for entire test suite"
  [test-fn]
  (let [db-file (setup-test-db)]
    (try 
      (with-redefs [db/db-spec {:dbtype "sqlite" :dbname db-file}]
        (test-fn))
      (finally 
        (cleanup-test-db)))))

(defn with-transaction-rollback
  "Fixture to wrap each test in a transaction that gets rolled back"
  [test-fn]
  (jdbc/with-transaction [tx (db/get-connection) {:rollback-only true}]
    (with-redefs [db/get-connection (constantly tx)]
      (test-fn))))