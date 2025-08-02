(ns pfm.db
  (:require [next.jdbc :as jdbc]
            [clojure.java.io :as io]))

(def db-spec {:dbtype "sqlite" :dbname "db.test.sqlite"})

(defn get-connection []
  (jdbc/get-datasource db-spec))

(defn query [sql]
  (jdbc/execute! (get-connection) [sql]))

(defn create-transactions-table! []
  (let [migrations-dir (io/file (io/resource "migrations"))
        sql-files (sort (.list migrations-dir))
        sql (slurp (io/file migrations-dir (first sql-files)))]
    (jdbc/execute! (get-connection) [sql])))