(ns cubes.app-test
  (:require-macros [test.helper :as helper])
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures]]
            [cljs.reader :as reader]
            [cljs-react-test.simulate :as sim]
            [cljs-react-test.utils :as tu]
            [dommy.core :as dommy :refer-macros [sel1 sel]]
            [om.next :as om :refer-macros [defui]]
            [cubes.squares :as sq]
            [cubes.app :as c]))

;; ======================================================================
;; Logic

(comment
  (def db (reader/read-string (helper/load-edn "test/resources/sample-db.edn")))

  (deftest plan-moves
    (let [goal [11 9]
          plan [{:type :move, :move 11, :to 9}]]
      (is (= plan (sq/plan-moves goal db))))))

;; ======================================================================
;; DOM

(def ^:dynamic c)

(use-fixtures :each (fn [test-fn]
                      (binding [c (tu/new-container!)]
                        (test-fn)
                        (tu/unmount! c))))

(deftest goal
  (doseq [state (->> (helper/load-edn "test/resources/predictive/states/cubes.edn")
                     (mapv reader/read-string))]
    (let [reconciler (om/reconciler {:state (atom state)
                                     :parser c/parser})]
      (om/add-root! reconciler c/GoalDescription c)
      (let [goal-text (.-innerHTML (sel1 c [:p]))]
        (is (= (map str (:goal state)) (re-seq #"\d+" goal-text)))))))
