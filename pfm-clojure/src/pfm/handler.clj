(ns pfm.handler
  (:require [pfm.db.transaction :as tx]
            [pfm.db :as db]
            [clojure.data.json :as json]))


(defn health-check [request]
  {:status 200
   :body "OK"})

(defn get-transactions [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (json/write-str (tx/get-all-transactions db/db-spec))})

(defn app [request]
  (case (:uri request)
    "/health" (health-check request)
    "/api/transactions" (get-transactions request)
    {:status 404
     :body "Not found"}))