(ns cubes.app-test
  (:require-macros [facilier.helper :as helper])
  (:require [cljs.test :refer-macros [deftest testing is use-fixtures async]]
            [cljs-react-test.simulate :as sim]
            [cljs-react-test.utils :as tu]
            [dommy.core :as dommy :refer-macros [sel1 sel]]
            [om.next :as om :refer-macros [defui]]
            [facilier.client :as f]
            [cubes.app :as c]))

;; ======================================================================
;; DOM Test

;; super ugly async work. Should definitely use a different test definition

(deftest predictive-testing
  (async done
         (let [c (tu/new-container!)]
           (f/get-state "cubes"
                        (fn [states]
                          (doseq [state states]
                            (let [reconciler (om/reconciler {:state (atom state)
                                                             :parser c/parser})]
                              (om/add-root! reconciler c/Widget c)
                              (testing "The goal is rendered"
                                (let [goal (:goal state)
                                      goal-text (.-innerHTML (sel1 c [:p.goal]))
                                      digits (re-seq #"\d+" goal-text)]
                                  (is (or (and (empty? goal) (nil? digits))
                                          (= (map str goal) digits))
                                      "All the blocks in the goal are in the text in order")))
                              (testing "The plan is rendered"
                                (let [plan-in-state? (not (empty? (:plan state)))
                                      plan-rendered? (some? (sel1 c [:ul.ops-list]))]
                                  (is (= plan-in-state? plan-rendered?)
                                      "The plan is rendered only if it is in the state")))))
                          (tu/unmount! c)
                          (done))))))
