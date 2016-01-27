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
          id "test"
          c (component/start (server/new-system {:port test-port}))
          state "asdfasdfadsf"]
      (is (= 200 (:status (client/delete (->dir test-port "state/test")))))
      (is (= 200 (:status (client/post (->dir test-port "state/test")
                                       {:body (pr-str state)}))))
      (is (= state (-> (->dir test-port "state/test")
                       client/get
                       :body
                       edn/read-string)))
      (component/stop c))))
