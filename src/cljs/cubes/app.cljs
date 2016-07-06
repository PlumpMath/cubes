(ns cubes.app
  (:require [rum.core :as rum :refer-macros [defc defcs]]
            [datascript.core :as d]
            [goog.style :as gstyle]
            [cubes.squares :as sq]
            [clojure.string :as str]))

(enable-console-print!)

;; ======================================================================
;; Constants

(def grid-size [600 600])

(def frame-rate 60)

;; ======================================================================
;; Geometry Helpers

(defn xy->xy
  "Coordinate transformation from regular cartesian to screen cartesian:
   (x', y') = (x, H - y)"
  [sq]
  (update sq :y #(- (first grid-size) (:side sq) %)))

;; In this case the inverse is the same
(def xy<-xy xy->xy)

;; ======================================================================
;; State

(defonce app-state
  (let [schema {:supports {:db/cardinality :db.cardinality/many
                           :db/valueType :db.type/ref}}
        db (-> (d/empty-db schema)
               (d/db-with [[:db/add -1 :screen-width (first grid-size)]
                           [:db/add -1 :screen-height (second grid-size)]])
               (sq/stack-squares 10))]
    (atom {:db db
           :ops []
           :tree []
           :goal []})))

(defmulti state->render
  "Takes the state, an op, the current frame,
  and returns what is necessary for rendering: claw and squares"
  (fn [_ op _] (:type op)))

(defmethod state->render :default [db _ _]
  {:squares (sq/db->squares db)})

(defn sq->target [sq target-sq f]
  (let [target-sq' (assoc target-sq :y (sq/sq->top target-sq))
        [x y] ((sq/rect-path sq target-sq') f)]
    (assoc sq :x x :y y)))

(defn move-sq [db op f]
  (sq->target (sq/get-sq db (:move op)) (sq/get-sq db (:to op)) f))

(defmethod state->render :claw [db op f]
  {:claw (move-sq db op f)
   :squares (sq/db->squares db)})

(defmethod state->render :move [db op f]
  (let [{:keys [x y db/id] :as sq'} (move-sq db op f)]
    {:claw sq'
     :squares (sq/db->squares (d/db-with db [[:db/add id :x x]
                                             [:db/add id :y y]]))}))

(defmethod state->render :get-rid-of [db op f]
  (let [sq (sq/get-sq db (:move op))
        fake-sq {:side (:side sq) :y 0 :x (sq/find-clear-space db (:side sq))}
        {:keys [x y db/id] :as sq'} (sq->target sq fake-sq f)]
    {:claw sq'
     :squares (sq/db->squares (d/db-with db [[:db/add id :x x]
                                             [:db/add id :y y]]))}))

(defn add-claw-moves
  "Add intermediate claw moves to the plan (for rendering)"
  [plan]
  (letfn [(add-claw-move [prev {:keys [move]}]
            (let [to (:move prev)]
              (cond-> [prev]
                (not= to move) (conj {:type :claw :to move :move to}))))]
    (concat (->> (drop 1 (cycle plan))
                 (mapcat add-claw-move (drop-last 1 plan)))
            [(last plan)])))

;; ======================================================================
;; State Transitions

(defn goal->op [[move to]]
  {:type :put :move move :to to})

(defn goal->moves
  "Expands a goal [from to] into a vector of moves to accomplish them"
  [db goal]
  (let [[from to] goal]
    (when-not (= from to)
      (let [plan (sq/expand-ops db [(goal->op goal)])]
        (if (sq/valid-plan? db plan)
          (add-claw-moves plan)
          ;; XXX: should throw
          (do (println "NOT VALID")
              (println plan)
              []))))))

(defn update-plan
  "Updates the ops and tree to the new goal"
  [{:keys [db goal] :as s}]
  (assoc s
         :frame 0
         :ops (goal->moves db goal)
         :tree (sq/expand-tree db (goal->op goal))))

(defn step-frame
  "If there are any operations left it steps the state one frame and returns it"
  [s]
  (if-let [op (first (:ops s))]
    (if (> 1 (:frame s))
      (assoc s :frame (+ (:frame s) (/ 1 frame-rate)))
      (cond-> (assoc s :frame 0 :ops (rest (:ops s)))
        (some? op) (update :db #(sq/step-op % op))))
    s))

(defn step-state!
  "Steps the app-state every 16ms as long as there are operations left"
  []
  (let [{:keys [db ops frame]} @app-state]
    (when-let [op (first ops)]
      (swap! app-state step-frame)
      (js/setTimeout step-state! 16))))

;; ======================================================================
;; Events

;; All events are raised! and dispatched centrally

(defmulti raise! (fn [[dispatch _]] dispatch))

(defmethod raise! :default [[action _]]
  (throw (js/Error. (str "Handler for " action " not defined"))))

(let [c (atom true)
      c (fn [] (swap! c not))]
  (defmethod raise! :square/click [[_ {:keys [db/id]}]]
    (swap! app-state
           #(assoc-in % [:goal (if (c) 1 0)] id))))

;; frame to 0
(defmethod raise! :square/start [_]
  (swap! app-state update-plan)
  (step-state!))

;; ======================================================================
;; UI

(defc icon < rum/static
  [{:keys [expanded? click-fn]}]
  [:a {:on-click (if (fn? click-fn) click-fn identity)}
   (if expanded?
     "▾"
     "►")])

(declare operations)

(defc operation [{:keys [op ops]}]
  [:li.op-item
   (sq/op->sentence op)
   (when-not (empty? ops)
     (operations ops))])

(defc operations < rum/static [ops]
  [:ul.ops-list {}
   (for [i (range (count ops))]
     (let [r (nth ops i)]
       (if (map? r)
         (let [base-op r]
           (rum/with-key (operation {:op base-op :ops []}) (str i)))
         (let [[op ops] r]
           (rum/with-key (operation {:op op :ops ops}) (str i))))))])

(defn root-ops [tree]
  (when-not (empty? tree)
    (let [[op ops] tree]
      (when-not (empty? ops)
        (operations ops)))))

(defc goal-description < rum/static [[sq tsq]]
  [:button.goal-description {:on-click (fn [_]
                                         (raise! [:square/start]))}
   (str "Move " (or sq "_") " to " (or tsq "_"))])

(defc square < rum/static [sq]
  (let [sq' (xy->xy sq)
        {:keys [side rgb db/id x y]} sq']
    [:g {:on-click (fn [_]
                     (raise! [:square/click {:db/id id}]))}
     [:rect {:height side :width side :x x :y y
             :style {:fill (str "rgb(" (str/join "," rgb) ")")}}]
     (let [[x y] (sq/sq->center sq')]
       [:text {:x x :y y} (str id)])]))

(def claw-width 10)

(defc grip < rum/static [{:keys [x y side]}]
  [:g
   [:rect {:height y
           :width claw-width
           :x (- (+ x (/ side 2)) (/ claw-width 2))
           :y 0
           :style {:fill "black"}}]
   [:rect {:height claw-width
           :width (* 4 claw-width)
           :x x
           :y (- y claw-width)
           :style {:fill "black"}}]])

(defc svg < rum/static [{:keys [db ops frame]}]
  (let [op (first ops)
        {:keys [squares claw]} (state->render db op frame)]
    (let [[height width] grid-size]
      [:svg {:height height :width width}
       [:rect {:height height :width width
               :style {:fill "white"
                       :stroke-width "2px"
                       :border-radius "2px"
                       :stroke "#888"}}]
       (when (and (:x claw) (:y claw))
         (grip (xy->xy claw)))
       (for [sq squares]
         (rum/with-key (square sq) (:db/id sq)))])))

(defc app-view < rum/reactive []
  (let [{:keys [goal tree] :as state} (rum/react app-state)]
    [:div {}
     [:div.container {}
      [:div.top {}
       (goal-description goal)]
      [:div.top-left {}
       (root-ops tree)]
      (svg state)]]))

(defn init []
  (rum/mount (app-view) (. js/document (getElementById "container"))))
