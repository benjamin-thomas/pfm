(ns pfm.handler)

(defn health-check [request]
  {:status 200
   :body "OK"})

(defn app [request]
  (case (:uri request)
    "/health" (health-check request)
    {:status 404
     :body "Not found"}))