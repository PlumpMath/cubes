(ns facilier.client
  "Helpers to log states from the client"
  (:require [ajax.core :refer [GET POST]]))

(def test-server-url "http://localhost:3005")

(defn post-state! [app-name state]
  (POST (str test-server-url "/state/" app-name)
        {:params {:state (pr-str state)}
         :format :edn
         :response-format :edn
         :handler (fn [_] (println "State recorded"))
         :error-handler (fn [e] (println "Recording failed: " e))}))

(defn log-states! [app-name ref]
  (add-watch ref ::states
             (fn [_ _ old-state new-state]
               (when-not (= old-state new-state)
                 (post-state! app-name new-state))))
  ref)
