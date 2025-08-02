(ns pfm.handler
  (:require [pfm.db.transaction :as tx]
            [pfm.db :as db]
            [clojure.data.json :as json]))

;; Pure handlers (no dependencies)
(defn health-check [request]
  {:status 200
   :body "OK"})

;; Handler factories (accept dependencies)
(defn make-get-transactions-handler [get-all-transactions-fn]
  (fn [request]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/write-str (get-all-transactions-fn))}))

;; App factory - creates the main app with injected dependencies
(defn make-app [deps]
  (let [get-transactions (make-get-transactions-handler (:get-all-transactions deps))]
    (fn [request]
      (case (:uri request)
        "/health" (health-check request)
        "/api/transactions" (get-transactions request)
        {:status 404
         :body "Not found"}))))

;; Default app with real dependencies for production use
(def app (make-app {:get-all-transactions #(tx/get-all-transactions db/db-spec)}))