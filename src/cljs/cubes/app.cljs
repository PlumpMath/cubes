(ns cubes.app
  (:require [rum.core :as rum :refer-macros [defc defcs]]
            [quil.core :as q :include-macros true]
            [datascript.core :as d]
            [goog.style :as gstyle]
            ;;            [facilier.client :as f]
            [cubes.squares :as sq]
            [clojure.string :as str]))

(enable-console-print!)

;; ======================================================================
;; Constants

(def grid-size [600 600])

(def frame-rate 20)

;; ======================================================================
;; Quil Helpers

(defn clear-canvas!
  "Draws a grey rectangle covering the whole canvas"
  []
  (q/fill 192 192 192)
  (apply q/rect 0 0 grid-size))

(defn xy->xy
  "Coordinate transformation from regular cartesian to screen cartesian:
   (x', y') = (x, H - y)"
  [sq]
  (update sq :y #(- (first grid-size) (:side sq) %)))

;; In this case the inverse is the same
(def xy<-xy xy->xy)

(defn sq-text!
  "Paints text inside the square"
  [sq]
  (q/stroke 0 0 0)
  (q/stroke-weight 0)
  (q/fill 0 0 0)
  (let [[x y] (sq/sq->center sq)]
    (q/text (:db/id sq) x y)))

(defn square!
  "Draws a square in the screen"
  [sq]
  (let [{:keys [x y side rgb] :as sq'} (xy->xy sq)]
    (apply q/fill rgb)
    (q/rect x y side side)
    (sq-text! sq')))

(defn claw!
  "Draws a claw at x up to y"
  [x y]
  (q/fill 0 0 0)
  (q/rect (- x 10) (- y 5) 25 5)
  (q/rect x 0 5 y))

(defn grip!
  "Draws a claw to a square"
  [sq]
  (let [{:keys [x y side]} (xy->xy sq)]
    (claw! (+ x (/ side 2)) y)))

;; ======================================================================
;; Render State

(defonce app-state
  (let [schema {:supports {:db/cardinality :db.cardinality/many
                           :db/valueType :db.type/ref}}
        db (-> (d/empty-db schema)
               (d/db-with [[:db/add -1 :screen-width (first grid-size)]
                           [:db/add -1 :screen-height (second grid-size)]])
               (sq/stack-squares 10))]
    (atom {:db0 db
           :plan []
           :tree []
           :goal []})))

;; (f/log-states! "cubes" app-state)

(defn init-draw-state [s]
  {:db (:db0 s) :ops [] :frame 0})

(defonce draw-state
  (atom (init-draw-state @app-state)))

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
;; Initialize and Render

(defn reset-draw-state!
  "Takes the app-state and resets the draw-state"
  [s]
  (reset! draw-state {:db (:db0 s) :ops (:plan s) :frame 0}))

(defn goal->op [[move to]]
  {:type :put :move move :to to})

(defn goal->moves [db goal]
  (when (apply not= goal)
    (let [plan (sq/expand-ops db [(goal->op goal)])]
      (if (sq/valid-plan? db plan)
        (add-claw-moves plan)
        (do (println "NOT VALID")
            (println plan)
            [])))))

(defn update-plan
  "Updates the plan and static db to the current goal and db"
  [s draw-state]
  (let [draw-db (:db draw-state)]
    (-> s
        (assoc :db0 draw-db
               :plan (goal->moves draw-db (:goal s))
               :tree (sq/expand-tree draw-db (goal->op (:goal s)))))))

(defn setup! []
  (q/frame-rate frame-rate)
  (q/color-mode :rgb)
  (q/background "#eee"))

(defn step-frame
  "If there are any operations left it steps the state one frame"
  [s]
  (if-let [op (first (:ops s))]
    (if (> 1 (:frame s))
      (assoc s :frame (+ (:frame s) (/ 1 frame-rate)))
      (cond-> (assoc s :frame 0 :ops (rest (:ops s)))
        (some? op) (update :db #(sq/step-op % op))))
    s))

(defn draw! []
  (clear-canvas!)
  (let [{:keys [db ops frame]} @draw-state
        op (first ops)
        {:keys [squares claw]} (state->render db op frame)]
    (doseq [sq squares]
      (square! sq))
    (when (and (:x claw) (:y claw))
      (grip! claw))
    (when (some? op)
      (swap! draw-state step-frame))))

(defn cubes-sketch! []
  (q/sketch
   :title "Oh so many grey circles"
   :host "canvas"
   :settings #(q/smooth 2) ;; Turn on anti-aliasing
   :setup setup!
   :draw draw!
   :size grid-size))

;; ======================================================================
;; DOM

(defn click->coords
  "Finds in which square was the click made (if any)"
  [e]
  (letfn [(->xy [ele]
            (let [c (gstyle/getClientPosition ele)]
              (map int [(.-x c) (.-y c)])))]
    (let [[x y] (map - (->xy e) (->xy (.-target e)))
          {:keys [x y]} (xy<-xy {:x x :y y :side 0})]
      [x y])))


(defmulti raise! (fn [[dispatch _]] dispatch))

(defmethod raise! :default [[action _]]
  (throw (js/Error. (str "handler for " action " not defined"))))

(let [c (atom true)
      c (fn [] (swap! c not))]
  (defmethod raise! :square/click [[_ {:keys [db/id]}]]
    (swap! app-state
           #(assoc-in % [:goal (if (c) 1 0)] id))))

(defmethod raise! :square/reset [_]
  (swap! app-state #(assoc % :db (:db0 %))))

(defmethod raise! :square/start [_]
  (let [s' (update-plan @app-state @draw-state)]
    (reset-draw-state! s')
    (reset! app-state s')))

(defc canvas < {:did-mount (fn [_]
                             (cubes-sketch!)
                             nil)}
  [db]
  [:canvas {:id "canvas"
            :height 600
            :width 900}])

(defc icon < rum/static
  [{:keys [expanded? click-fn]}]
  [:a {:on-click (if (fn? click-fn) click-fn identity)}
   (if expanded?
     "▾"
     "►")])

(declare operations)

(defcs operation < (rum/local {:expanded? false :selected? false} :toggle)
  [{:keys [toggle]} {:keys [op ops]}]
  (let [{:keys [expanded? selected?]} @toggle]
    [:li.op-item
     [:span {:class (str "clickable "
                         (if selected?
                           "op-item__text--activated"
                           "op-item__text"))
             :title (if selected? "Collapse" "Expand")}
      (when-not (empty? ops)
        (icon {:expanded? expanded?
               :click-fn (fn [_]
                           (swap! toggle #(update % :expanded? not)))}))
      (sq/op->sentence op)
      [:.divider]
      (when (and expanded? (not (empty? ops)))
        (operations ops))]]))

(defc operations < rum/static [ops]
  [:ul.ops-list {}
   (for [i (range (count ops))]
     (let [v (nth ops i)
           [op ops] v]
       (when (and (some? op))
         (rum/with-key (operation {:op op :ops ops}) (str i)))))])

(defn root-ops [tree]
  (when-not (empty? tree)
    (let [[op ops] tree]
      (when-not (empty? ops)
        (operations ops)))))

(defc goal-description < rum/static [[sq tsq]]
  [:p.goal (str "Move " (or sq "_") " to " (or tsq "_"))])

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
   [:rect {:height y :width claw-width
           :x (- (+ x (/ side 2)) (/ claw-width 2))
           :y 0
           :style {:fill "black"}}]
   [:rect {:height claw-width
           :width (* 4 claw-width)
           :x x
           :y (- y claw-width)
           :style {:fill "black"}}]])

(defcs svg < rum/reactive []
  (let [{:keys [db ops frame]} (rum/react draw-state)
        op (first ops)
        {:keys [squares claw]} (state->render db op frame)]
    [:svg {:height (first grid-size) :width (second grid-size)}
     (when (and (:x claw) (:y claw))
       (grip (xy->xy claw)))
     (for [sq squares]
       (square sq))]))

(defc app-view < rum/reactive []
  (let [{:keys [goal db0 tree]} (rum/react app-state)]
    [:div {}
     [:div {}
      (goal-description goal)
      [:button {:on-click (fn [_]
                            (raise! [:square/reset]))}
       "Reset"]
      [:button {:on-click (fn [_]
                            (raise! [:square/start]))}
       "Start"]
      [:br]
      (canvas {})
;;      (root-ops tree)
      (svg)
  ]]))

(defn init []
  (rum/mount (app-view) (. js/document (getElementById "container"))))
