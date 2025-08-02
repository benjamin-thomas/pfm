(ns e2e.e2e-test
  (:require [clojure.test :refer :all]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [ring.adapter.jetty :as jetty]
            [pfm.handler :as handler]
            [pfm.core :as core]
            [pfm.db :as db]
            [pfm.db.transaction :as tx]
            [pfm.db.migrations :as migrations]))

(def ^:dynamic *base-url* nil)
(def ^:dynamic *server* nil)

(defn with-test-server [test-fn]
  "Fixture to start and stop server for E2E tests"
  ;; Set environment variables for test  
  (System/setProperty "APP_ENV" "test")
  
  ;; Override get-env! to use system properties instead
  (with-redefs [core/get-env! (fn [key]
                                (case key
                                  "APP_ENV" "test"
                                  "PORT" "0"  ; Will be ignored since we use Jetty's port detection
                                  (throw (ex-info (str "Missing required environment variable: " key)
                                                  {:missing-env-var key}))))]
    ;; Setup database for E2E tests
    (let [test-db-name "pfm.test.db"]
      ;; Clean slate - delete existing test database
      (let [db-file (clojure.java.io/file test-db-name)]
        (when (.exists db-file)
          (.delete db-file)))
      
      ;; Set up database and insert fixtures
      (alter-var-root #'db/db-spec (constantly {:dbtype "sqlite" :dbname test-db-name}))
      (migrations/run-migrations)
      (tx/insert-transaction! db/db-spec {:description "Coffee Shop" :amount 4.50 :created_at_unix 1234567890})
      (tx/insert-transaction! db/db-spec {:description "Grocery Store" :amount 23.45 :created_at_unix 1234567891})
      (tx/insert-transaction! db/db-spec {:description "Gas Station" :amount 45.00 :created_at_unix 1234567892})
      
      
      ;; Start server on system-assigned port (port 0 = any available port)
      (let [server (jetty/run-jetty handler/app {:port 0 :join? false})
            actual-port (.getLocalPort (first (.getConnectors server)))
            base-url (str "http://localhost:" actual-port)]
        (try
          ;; Give server a moment to start
          (Thread/sleep 500)
          (binding [*server* server
                    *base-url* base-url]
            (test-fn))
          (finally
            (.stop server)
            ;; Clean up system properties
            (System/clearProperty "APP_ENV")))))))

(use-fixtures :once with-test-server)

(deftest server-e2e-test
  (testing "Full E2E test with real server and HTTP client"
    ;; Test health endpoint
    (let [response (http/get (str *base-url* "/health"))]
      (is (= 200 (:status response)))
      (is (= "OK" (:body response))))
    
    ;; Test API endpoint returns fixture data
    (let [response (http/get (str *base-url* "/api/transactions"))
          parsed-json (json/read-str (:body response) :key-fn keyword)]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers :content-type])))
      (is (= 3 (count parsed-json)))
      (is (= "Coffee Shop" (:description (first parsed-json))))
      (is (= 4.5 (:amount (first parsed-json))))
      (is (= "Grocery Store" (:description (second parsed-json))))
      (is (= 23.45 (:amount (second parsed-json)))))))