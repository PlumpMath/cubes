(ns cubes.app-test
  (:require-macros [test.helper :as helper])
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures]]
            [cljs.reader :as reader]
            [cljs-react-test.simulate :as sim]
            [cljs-react-test.utils :as tu]
            [dommy.core :as dommy :refer-macros [sel1 sel]]
            [om.next :as om :refer-macros [defui]]
            [cubes.app :as c]))

;; ======================================================================
;; DOM Test

(def ^:dynamic c)

(use-fixtures :each (fn [test-fn]
                      (binding [c (tu/new-container!)]
                        (test-fn)
                        (tu/unmount! c))))

(deftest predictive-testing
  (doseq [state (helper/load-states "cubes")]
    (let [reconciler (om/reconciler {:state (atom state)
                                     :parser c/parser})]
      (om/add-root! reconciler c/Widget c)
      (testing "The goal is rendered"
        (let [goal-text (.-innerHTML (sel1 c [:p]))]
          (is (= (map str (:goal state)) (re-seq #"\d+" goal-text))
              "All the blocks in the goal are in the text in order")))
      (testing "The plan is rendered"
        (let [plan-in-state? (not (empty? (:plan state)))
              plan-rendered? (some? (sel1 c [:ul.ops-list]))]
          (is (= plan-in-state? plan-rendered?)
              "The plan is rendered only if it is in the state"))))))
