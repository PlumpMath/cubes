(ns test.test.server
  (:require [clojure.test :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [clj-http.client :as client]
            [com.stuartsierra.component :as component]
            [test.server :as server]))

(defn ->dir [test-port url]
  (str "http://localhost:" test-port "/" url))

(deftest save-state
  (testing "The server can save states to file and return them"
    (let [test-port 3006
          c (component/start (server/new-system {:port test-port}))
          state "asdfasdfadsf"]
      (is (= 200 (:status (client/delete (->dir test-port "state")))))
      (is (= 200 (:status (client/post (->dir test-port "state")
                                       {:body (pr-str {:state state})}))))
      (is (= state (-> (->dir test-port "state") client/get :body edn/read-string)))
      (component/stop c))))
