(ns pfm.handler)

(defn health-check [request]
  {:status 200
   :body "OK"})

(defn get-transactions [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body []})

(defn app [request]
  (case (:uri request)
    "/health" (health-check request)
    "/api/transactions" (get-transactions request)
    {:status 404
     :body "Not found"}))