(ns test.server
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [com.stuartsierra.component :as component]
            [ring.middleware.params :as params]
            [ring.util.response :as response]
            [ring.adapter.jetty :as jetty]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.handler :as handler]))

;; ======================================================================
;; Helpers

(defn ok-response [body]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (pr-str body)})

(defn error-response [error]
  {:status 500
   :headers {"Content-Type" "application/edn"}
   :body (pr-str error)})

;; ======================================================================
;; Logic

(def root-dir "test/resources/predictive/states")

(def state "{:db0 #datascript/DB {:schema {:supports {:db/cardinality :db.cardinality/many, :db/valueType :db.type/ref}}, :datoms [[1 :screen-height 600 536870913] [1 :screen-width 600 536870913] [2 :rgb [149 52 25] 536870914] [2 :side 40 536870914] [2 :supports 4 536870916] [2 :x 518 536870914] [2 :y 0 536870914] [3 :rgb [35 90 26] 536870915] [3 :side 40 536870915] [3 :x 443 536870915] [3 :y 0 536870915] [4 :rgb [92 246 182] 536870916] [4 :side 40 536870916] [4 :x 518 536870916] [4 :y 40 536870916] [5 :rgb [70 105 27] 536870917] [5 :side 40 536870917] [5 :supports 7 536870919] [5 :x 374 536870917] [5 :y 0 536870917] [6 :rgb [117 246 223] 536870918] [6 :side 40 536870918] [6 :x 55 536870918] [6 :y 0 536870918] [7 :rgb [231 112 35] 536870919] [7 :side 40 536870919] [7 :supports 11 536870923] [7 :x 374 536870919] [7 :y 40 536870919] [8 :rgb [222 114 76] 536870920] [8 :side 40 536870920] [8 :supports 9 536870921] [8 :x 256 536870920] [8 :y 0 536870920] [9 :rgb [109 111 84] 536870921] [9 :side 40 536870921] [9 :x 256 536870921] [9 :y 40 536870921] [10 :rgb [59 200 97] 536870922] [10 :side 40 536870922] [10 :x 168 536870922] [10 :y 0 536870922] [11 :rgb [198 63 125] 536870923] [11 :side 40 536870923] [11 :x 374 536870923] [11 :y 80 536870923]]}, :plan [], :tree [], :goal [9 6], :om.next/queries nil}")

(defn save-state! [session-id state]
  (spit (io/file root-dir (str session-id ".edn")) :append true))

;; ======================================================================
;; HTTP Service

(defn handle-state! [params]
  (let [{:keys [session-id state]} params]
    (try
      (save-state! "uuid" state)
      (ok-response {:ok "saved"})
      (catch Exception e
        (error-response e)))))

(defroutes app-routes
  (GET "/" [] (ok-response "<h1>YES</h1>"))
  (POST "/state" {:keys [params]} (handle-state! params)))

(def app-handler
  (-> app-routes
      handler/site))

(defn start-jetty [handler port]
  (jetty/run-jetty handler {:port (Integer. port) :join? false}))

(defrecord Server [port jetty]
  component/Lifecycle
  (start [component]
    (println "Start server at port " port)
    (assoc component :jetty (start-jetty app-handler port)))
  (stop [component]
    (println "Stop server")
    (when jetty
      (.stop jetty))
    component))

(defn new-system [opts]
  (Server. 3005 nil))
