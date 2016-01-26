(ns cubes.app
  (:require [om.next :as om :refer-macros [defui]]
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

;; in this watch send to a server that serializes and stores in files
(defonce watch
  (add-watch app-state nil (fn [_ _ o n]
                             (println (pr-str n)))))

(defn init-draw-state [s]
  {:db (:db0 s) :ops [] :frame 0})

(defonce draw-state
  (atom (init-draw-state @app-state)))

(defn read
  [{:keys [state] :as env} key params]
  (if-let [[_ value] (find @state key)]
    {:value value}
    {:value nil}))

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
  (q/background 200))

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

(defmulti mutate om/dispatch)

(defmethod mutate :default [_ _ _] {:value []})

(let [c (atom true)
      c (fn [] (swap! c not))]
  (defmethod mutate `square/click
    [{:keys [state]} key {:keys [coords]}]
    {:value {:keys [:goal]}
     :action (fn []
               (swap! state #(if-let [sq (sq/coords->sq (:db @draw-state) coords)]
                               (assoc-in % [:goal (if (c) 1 0)] sq)
                               %)))}))

(defmethod mutate `square/reset
  [{:keys [state]} key params]
  {:value {:keys [:goal]}
   :action (fn [] (swap! state #(assoc % :db (:db0 %))))})

(defmethod mutate `square/start
  [{:keys [state]} key params]
  {:value {:keys [:goal]}
   :action (fn []
             (let [s' (update-plan @state @draw-state)]
               (reset-draw-state! s')
               (reset! state s')))})

(defui Canvas
  static om/IQuery
  (query [_] '[:db])
  Object
  (render [this]
    (let [{:keys [db]} (om/props this)]
      (dom/canvas #js {:id "canvas" :height 600 :widht 900
                       :onClick
                       (fn [e]
                         (when-let [coords (click->coords e)]
                           (om/transact! this
                                         `[(square/click {:coords ~coords})])))})))
  (componentDidMount [_]
    (cubes-sketch!)))

(def canvas (om/factory Canvas))

(defn icon
  [{:keys [icon-class title click-fn]}]
  (dom/i #js {:className (str icon-class " fa clickable")
              :title title
              :onClick (if (fn? click-fn) click-fn identity)}))

(declare operations)

(defui Operation
  static om/IQuery
  (query [_] '[:selected :op :ops])
  Object
  #_(getInitialState [_] {:expand? false})
  (render [this]
    (let [{:keys [expand?]} (om/get-state this)
          {:keys [selected op ops]} (om/props this)
          selected? (contains? selected op)]
      (dom/li #js {:className "file-item"}
              (dom/span #js {:className (str "clickable "
                                             (if selected?
                                               "file-item__text--activated"
                                               "file-item__text"))
                             :title (if selected? "Collapse" "Expand")}
                        (sq/op->sentence op))
              #_(when-not (empty? ops)
                (icon {:title "Expand directory"
                       :icon-class (str "fa-chevron-down "
                                        (if expand?
                                          "expand-icon--active"
                                          "expand-icon"))
                       :click-fn (fn [_]
                                   (om/update-state! this update :expand? not))}))
              #_(dom/div #js {:className "divider"} nil)
              #_(when (and expand? (not (empty? ops)))
                (operations {:ops ops}))))))

(def operation (om/factory Operation))

(defui Operations
  om/IQuery
  (query [_] [:ops])
  Object
  (render [this]
          (let [{:keys [ops]} (om/props this)]
            (apply dom/ul #js {:className "folder-list"}
                   (map (fn [v]
                          (let [[op ops] v]
                            (when (and (some? op)  (not (empty? ops)))
                              (operation {:op op :ops ops}))))
                        ops)))))

(def operations (om/factory Operations))

(defui GoalDescription
  static om/IQuery
  (query [_] '[:goal])
  Object
  (render [this]
          (let [{:keys [goal]} (om/props this)
                [sq tsq] goal]
            (dom/p nil (str "Move " (or sq "_")
                            " to " (or tsq "_"))))))

(def goal-description (om/factory GoalDescription))

(defui Widget
  static om/IQuery
  (query [_] '[:goal :db0 :db :tree])
  Object
  (render [this]
          (let [{:keys [goal db0 tree] :as data} (om/props this)]
            (dom/div nil
                     (dom/div nil
                              (goal-description {:goal goal})
                              (dom/button #js {:onClick (fn [_]
                                                          (om/transact! this '[(square/reset)]))}
                                          "Reset")
                              (dom/button #js {:onClick (fn [_]
                                                          (om/transact! this '[(square/start)]))}
                                          "Start"))
                     (canvas {})
                     (when-not (empty? tree)
                       (let [[op ops] tree]
                         (when-not (empty? ops)
                           (operations {:ops ops}))))))))

(def parser (om/parser {:read read :mutate mutate}))

(def reconciler
  (om/reconciler {:state app-state
                  :parser parser}))

(defn init []
  (om/add-root! reconciler Widget (. js/document (getElementById "container"))))
