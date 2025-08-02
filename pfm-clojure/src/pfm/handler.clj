(ns pfm.handler
  (:require [pfm.db :as db]))

(defn health-check [request]
  {:status 200
   :body "OK"})

(defn get-transactions [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (db/get-all-transactions)})

(defn app [request]
  (case (:uri request)
    "/health" (health-check request)
    "/api/transactions" (get-transactions request)
    {:status 404
     :body "Not found"}))