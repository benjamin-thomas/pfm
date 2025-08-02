(ns pfm.db
  (:require [clojure.java.jdbc :as jdbc]))

(def db-spec {:dbtype "sqlite" :dbname "db.test.sqlite"})

(defn get-connection []
  db-spec)

(defn query [sql]
  (jdbc/query db-spec [sql]))