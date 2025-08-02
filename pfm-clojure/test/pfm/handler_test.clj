(ns pfm.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pfm.handler :as handler]
            [clojure.data.json :as json]))

(deftest health-check-test
  (testing "GET /health should return OK"
    (let [response (handler/health-check {})]
      (is (= 200 (:status response)))
      (is (= "OK" (:body response)))))
  )

(deftest app-test
  (testing "GET /wat should return a 404"
    (let [test-app (handler/make-app {:get-all-transactions (constantly [])})
          response (test-app (mock/request :get "/wat"))]
      (is (= 404 (:status response)))
      (is (= "Not found" (:body response))))))

(deftest get-transactions-handler-test
  (testing "GET /api/transactions returns empty JSON when no transactions"
    (let [handler (handler/make-get-transactions-handler (constantly []))
          response (handler {})
          parsed-body (json/read-str (:body response))]
      (is (= 200 (:status response)))
      (is (= "application/json" (get-in response [:headers "Content-Type"])))
      (is (= [] parsed-body))))
  
  (testing "GET /api/transactions returns actual transaction data"
    (let [test-data [{:id 1 :description "Coffee" :amount 4.50 :created_at_unix 1234567890}
                     {:id 2 :description "Lunch" :amount 12.75 :created_at_unix 1234567891}]
          handler (handler/make-get-transactions-handler (constantly test-data))
          response (handler {})
          parsed-body (json/read-str (:body response) :key-fn keyword)]
      (is (= 200 (:status response)))
      (is (= 2 (count parsed-body)))
      (is (= "Coffee" (:description (first parsed-body))))
      (is (= 4.5 (:amount (first parsed-body))))
      (is (= "Lunch" (:description (second parsed-body)))))))

(deftest app-integration-test
  (testing "App routes requests correctly"
    (let [test-data [{:id 1 :description "Test"}]
          test-app (handler/make-app {:get-all-transactions (constantly test-data)})]
      
      (testing "health endpoint"
        (let [response (test-app (mock/request :get "/health"))]
          (is (= 200 (:status response)))
          (is (= "OK" (:body response)))))
      
      (testing "transactions endpoint"
        (let [response (test-app (mock/request :get "/api/transactions"))
              parsed-body (json/read-str (:body response) :key-fn keyword)]
          (is (= 200 (:status response)))
          (is (= test-data parsed-body)))))))
