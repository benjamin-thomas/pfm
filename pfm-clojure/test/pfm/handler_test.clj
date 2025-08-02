; clojure -M:test --watch
(ns pfm.handler-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [pfm.handler :as handler]))

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
      (is (= "Not found" (:body response)))))
  )
