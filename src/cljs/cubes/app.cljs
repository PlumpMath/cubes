(ns cubes.app
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [quil.core :as q :include-macros true]
            [datascript.core :as d]
            [goog.style :as gstyle]
            [cubes.squares :as sq]))

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
  (q/fill 0 0 0)
  (let [[x y] (sq/sq->center sq)]
    (q/text (:db/id sq) x y)))

(defn square!
  "Draws a square in the screen"
  [sq]
  (let [{:keys [x y side] :as sq'} (xy->xy sq)]
    (apply q/fill (sq/sq->rgb sq'))
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
  (atom {:db0 nil
         :plan []
         :db nil
         :ops []
         :frame 0}))

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

(defn reset-state [s]
  (assoc s :db (:db0 s) :ops (:plan s) :frame 0))

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
  [s]
  (-> s
      (assoc :db0 (:db s) :plan (goal->moves (:db s) (:goal s)))
      reset-state))

(defn setup! []
  (q/frame-rate frame-rate)
  (q/color-mode :rgb)
  (q/background 200)
  (let [schema {:supports {:db/cardinality :db.cardinality/many
                           :db/valueType :db.type/ref}}
        db (-> (d/empty-db schema)
               (d/db-with [[:db/add -1 :screen-width (first grid-size)]
                           [:db/add -1 :screen-height (second grid-size)]])
               (sq/stack-squares 10))]
    (reset! app-state (reset-state {:db0 db :plan [] :goal []}))))

(defn step-frame
  "If there are any operations left it steps the state one frame"
  [s]
  (if-let [op (first (:ops s))]
    (if (> 1 (:frame s))
      (assoc s :frame (+ (:frame s) (/ 1 frame-rate)))
      (cond-> s
        true (assoc :frame 0 :ops (rest (:ops s)))
        (some? op) (update :db #(sq/step-op % op))))
    s))

(defn draw! []
  (clear-canvas!)
  (let [{:keys [db ops frame]} @app-state
        op (first ops)
        {:keys [squares claw]} (state->render db op frame)]
    (doseq [sq squares]
      (square! sq))
    (when (and (:x claw) (:y claw))
      (grip! claw))
    (when (some? op)
      (swap! app-state step-frame))))

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

(defn click->sq [db e]
  (letfn [(->xy [c]
            (map int [(.-x c) (.-y c)]))]
    (let [t-coords (gstyle/getClientPosition (.-target e))
          e-coords (gstyle/getClientPosition e)
          [x y] (map - (->xy e-coords) (->xy t-coords))
          {:keys [x y]} (xy<-xy {:x x :y y :side 0})]
      (sq/coords->sq db [x y]))))

(let [c (atom true)
      c (fn [] (swap! c not))]
  (defn click-handler [e]
    (when-let [sq (click->sq (:db @app-state) e)]
      (swap! app-state #(assoc-in % [:goal (if (c) 1 0)] sq)))))

(defn canvas [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/canvas #js {:id "canvas" :height 600 :widht 900
                       :onClick click-handler}))
    om/IDidMount
    (did-mount [_]
      (cubes-sketch!))))

(defn icon-button [{:keys [icon-class title]} owner {:keys [click-fn]}]
  (om/component
    (dom/i #js {:className (str icon-class " fa clickable")
                :title title
                :onClick (if (fn? click-fn)
                           click-fn
                           identity)})))

(declare list-ops)

(defn op-item [{:keys [selected op ops]} owner {:keys [click-fn] :as opts}]
  (reify
    om/IInitState
    (init-state [_] {:expand? true})
    om/IRenderState
    (render-state [_ {:keys [expand?]}]
      (let [selected? (contains? selected op)]
        (dom/li #js {:className "file-item"}
          (dom/span #js {:className (str "clickable "
                                      (if selected?
                                        "file-item__text--activated"
                                        "file-item__text"))
                         :onClick (partial click-fn op)
                         :title (if selected?
                                  "Unselect directory"
                                  "Select directory")}
            (sq/op->sentence op))
          (when-not (empty? ops)
            (om/build icon-button
              {:title "Expand directory"
               :icon-class (str "fa-chevron-down "
                             (if expand?
                               "expand-icon--active"
                               "expand-icon"))}
              {:opts {:click-fn (fn [_]
                                  (om/update-state! owner :expand? not))}}))
          (dom/div #js {:className "divider"} nil)
          (when expand?
            (om/build list-ops {:op op :ops ops} {:opts opts})))))))

(defn list-ops [{:keys [op ops]} owner opts]
  (om/component
   (apply dom/ul #js {:className "folder-list"}
     (map-indexed
       (fn [i v]
         (cond
           (vector? v)
           (let [[op ops] v]
             (om/build op-item {:op op :ops ops} {:opts opts}))
           :else (om/build op-item {:op v :ops []} {:opts opts})))
       ops))))

(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil
        (dom/div nil
          (let [[sq tsq] (:goal data)]
            (dom/p nil (str "Move " sq " to " tsq)))
          (dom/button #js {:onClick (fn [_]
                                      (om/transact! data #(assoc % :db (:db0 %))))}
                      "Reset")
          (dom/button #js {:onClick (fn [_]
                                      (om/transact! data update-plan))}
                      "Start"))
        (om/build canvas {})
        (let [op (goal->op (:goal data))
              db (:db0 @app-state)]
          (when (and (d/db? db) (sq/valid-op? db op))
            (let [[op ops] (sq/expand-tree db op)]
              (om/build list-ops {:op op :ops ops}))))))))

(defn init []
  (om/root widget app-state
           {:target (. js/document (getElementById "container"))}))
