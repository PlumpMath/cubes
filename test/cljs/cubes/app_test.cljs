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
;; DOM Test

(def ^:dynamic c)

(use-fixtures :each (fn [test-fn]
                      (binding [c (tu/new-container!)]
                        (test-fn)
                        (tu/unmount! c))))

(deftest goal
  (doseq [state (helper/load-states "cubes")]
    (let [reconciler (om/reconciler {:state (atom state)
                                     :parser c/parser})]
      (testing "The goals is rendered"
        (om/add-root! reconciler c/GoalDescription c)
        (let [goal-text (.-innerHTML (sel1 c [:p]))]
          (is (= (map str (:goal state)) (re-seq #"\d+" goal-text))))))))

(deftest plans
  (doseq [state (helper/load-states "cubes")]
    (let [reconciler (om/reconciler {:state (atom state)
                                     :parser c/parser})]
      (testing "the Plans are rendered"
        (om/add-root! reconciler c/RootOps c)
        (let [plan-there? (not (empty? (:plan state)))
              plan-rendered? (some? (sel1 c [:ul.ops-list]))]
          (is (= plan-there? plan-rendered?)
              "If there is a plan, it should be rendered. No plan, no render"))))))
