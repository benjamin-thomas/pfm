(ns pfm.db.migrations
  (:require [clojure.java.jdbc :as jdbc]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [pfm.db :as db]))

(defn list-migration-files
  "List all .sql files in the migrations directory, sorted by filename"
  []
  (->> (io/resource "migrations")
       io/file
       file-seq
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) ".sql"))
       (sort-by #(.getName %))))

(defn read-migration-sql
  "Read the SQL content from a migration file"
  [file]
  (slurp file))

(defn run-migrations
  "Run all migrations in order from the migrations folder"
  []
  (println "Running database migrations...")
  (doseq [migration-file (list-migration-files)]
    (let [filename (.getName migration-file)
          sql-content (read-migration-sql migration-file)]
      (println (str "Executing migration: " filename))
      (jdbc/execute! db/db-spec [sql-content])
      (println (str "✓ Completed migration: " filename))))
  (println "All migrations completed successfully."))