(ns pfm.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pfm.handler :as handler]
            [pfm.db :as db]
            [pfm.test-helpers :as helpers]))

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
    (let [response (handler/get-transactions {})]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (is (= [] (:body response)))))
  
  (testing "GET /api/transactions returns actual transaction data"
    ;; Insert test data
    (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Coffee', 4.50, 1234567890)")
    (db/query "INSERT INTO transactions (description, amount, created_at_unix) VALUES ('Lunch', 12.75, 1234567891)")
    
    (let [response (handler/get-transactions {})
          body (:body response)]
      (is (= 200 (:status response)))
      (is (= 2 (count body)))
      (is (= "Coffee" (:transactions/description (first body))))
      (is (= 4.5 (:transactions/amount (first body))))
      (is (= "Lunch" (:transactions/description (second body)))))))
