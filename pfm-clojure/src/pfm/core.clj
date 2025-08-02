(ns pfm.core
  (:require [ring.adapter.jetty :as jetty]
            [pfm.handler :as handler]
            [pfm.db :as db])
  (:gen-class))

(defn get-env! 
  "Get required environment variable, crash if missing"
  [key]
  (or (System/getenv key)
      (throw (ex-info (str "Missing required environment variable: " key)
                      {:missing-env-var key}))))

(defn get-db-name []
  (case (get-env! "APP_ENV")
    "test" "pfm.test.db"
    "production" "pfm.production.db"
    "development" "pfm.dev.db"))

(defn setup-database! []
  (let [db-name (get-db-name)
        app-env (get-env! "APP_ENV")]
    (println (str "Setting up database: " db-name))
    
    ;; For test environment, always start with fresh database
    (when (= "test" app-env)
      (let [db-file (clojure.java.io/file db-name)]
        (when (.exists db-file)
          (.delete db-file)
          (println "Deleted existing test database"))))
    
    (with-redefs [db/db-spec {:dbtype "sqlite" :dbname db-name}]
      (db/create-transactions-table!)
      (println "Database migrations completed.")
      
      ;; Insert fixture data for test environment
      (when (= "test" app-env)
        (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Coffee Shop', 4.50, 1234567890)")
        (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Grocery Store', 23.45, 1234567891)")
        (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Gas Station', 45.00, 1234567892)")
        (println "Test fixture data inserted.")))))

(defn -main [& args]
  (let [port (Integer/parseInt (get-env! "PORT"))
        db-name (get-db-name)]
    (println (str "Starting PFM server on port " port "..."))
    (println (str "Environment: " (get-env! "APP_ENV")))
    
    ;; Setup database based on environment
    (setup-database!)
    
    ;; Permanently set the database spec for the server
    (alter-var-root #'db/db-spec (constantly {:dbtype "sqlite" :dbname db-name}))
    (println (str "Server using database: " db-name))
    
    ;; Start server
    (jetty/run-jetty handler/app {:port port :join? true})
    (println "Server stopped.")))