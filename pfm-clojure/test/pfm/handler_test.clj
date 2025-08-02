(ns pfm.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pfm.handler :as handler]
            [pfm.db :as db]
            [pfm.test-helpers :as helpers]
            [clojure.data.json :as json]))

(use-fixtures :once helpers/with-test-db-suite)
(use-fixtures :each helpers/with-transaction-rollback)

(deftest health-check-test
  (testing "GET /health should return OK"
    (let [response (handler/health-check {})]
      (is (= 200 (:status response)))
      (is (= "OK" (:body response)))))
  )

(deftest app-test
  (testing "GET /wat should return a 404"
    (let [response (handler/app (mock/request :get "/wat"))]
      (is (= 404 (:status response)))
      (is (= "Not found" (:body response))))))

(deftest get-transactions-test
  (testing "GET /api/transactions returns JSON"
    (let [response (handler/get-transactions {})
          parsed-body (json/read-str (:body response))]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (is (= [] parsed-body))))
  
  (testing "GET /api/transactions returns actual transaction data"
    ;; Insert test data
    (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Coffee', 4.50, 1234567890)")
    (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Lunch', 12.75, 1234567891)")
    
    (let [response (handler/get-transactions {})
          parsed-body (json/read-str (:body response) :key-fn keyword)]
      (is (= 200 (:status response)))
      (is (= 2 (count parsed-body)))
      (is (= "Coffee" (:description (first parsed-body))))
      (is (= 4.5 (:amount (first parsed-body))))
      (is (= "Lunch" (:description (second parsed-body)))))))
