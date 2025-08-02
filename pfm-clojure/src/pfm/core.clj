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
      (println "Database migrations completed."))))

(defn -main [& args]
  (let [port (Integer/parseInt (get-env! "PORT"))]
    (println (str "Starting PFM server on port " port "..."))
    (println (str "Environment: " (get-env! "APP_ENV")))
    
    ;; Setup database based on environment
    (setup-database!)
    
    ;; Start server
    (jetty/run-jetty handler/app {:port port :join? true})
    (println "Server stopped.")))