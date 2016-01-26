(ns cubes.app-test
  (:require-macros [test.helper :as helper])
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures]]
            [cljs.reader :as reader]
            [cljs-react-test.simulate :as sim]
            [cljs-react-test.utils :as tu]
            [cubes.squares :as sq]
            [cubes.app :as c]))

;; ======================================================================
;; Logic

(def db (reader/read-string (helper/load-edn "test/resources/sample-db.edn")))

(deftest plan-moves
  (let [goal [11 9]
        plan [{:type :move, :move 11, :to 9}]]
    (is (= plan (sq/plan-moves goal db)))))

;; ======================================================================
;; DOM

(def ^:dynamic c)

(use-fixtures :each (fn [test-fn]
                      (binding [c (tu/new-container!)]
                        (test-fn)
                        (tu/unmount! c))))
