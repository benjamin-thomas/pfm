(ns pfm.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pfm.handler :as handler]
            [pfm.db.transaction :as db.tx]
            [clojure.data.json :as json]))

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
  (testing "GET /api/transactions returns empty JSON when no transactions"
    (with-redefs [db.tx/get-all-transactions (constantly [])]
      (let [response (handler/get-transactions {})
            parsed-body (json/read-str (:body response))]
        (is (= 200 (:status response)))
        (is (= "application/json" (get-in response [:headers "Content-Type"])))
        (is (= [] parsed-body)))))
  
  (testing "GET /api/transactions returns actual transaction data"
    (let [test-data [{:id 1 :description "Coffee" :amount 4.50 :created_at_unix 1234567890}
                     {:id 2 :description "Lunch" :amount 12.75 :created_at_unix 1234567891}]]
      (with-redefs [db.tx/get-all-transactions (constantly test-data)]
        (let [response (handler/get-transactions {})
              parsed-body (json/read-str (:body response) :key-fn keyword)]
          (is (= 200 (:status response)))
          (is (= 2 (count parsed-body)))
          (is (= "Coffee" (:description (first parsed-body))))
          (is (= 4.5 (:amount (first parsed-body))))
          (is (= "Lunch" (:description (second parsed-body)))))))))
