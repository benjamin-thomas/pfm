(ns pfm.db
  (:require [next.jdbc :as jdbc]))

(def db-spec {:dbtype "sqlite" :dbname "db.test.sqlite"})

(defn get-connection []
  (jdbc/get-datasource db-spec))

(defn query [sql]
  (jdbc/execute! (get-connection) [sql]))