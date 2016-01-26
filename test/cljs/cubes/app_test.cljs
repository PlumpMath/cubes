(ns cubes.app-test
  (:require-macros [test.helper :as helper])
  (:require [cljs.test :refer-macros [deftest testing is]]
            [cljs.reader :as reader]
            [cubes.squares :as sq]
            [cubes.app :as c]))

(def db (reader/read-string (helper/load-edn "test/resources/sample-db.edn")))

(deftest plan-moves
  (let [goal [11 9]
        plan [{:type :move, :move 11, :to 9}]]
    (is (= plan (sq/plan-moves goal db)))))
