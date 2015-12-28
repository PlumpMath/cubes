(ns cubes.app
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [quil.core :as q :include-macros true]
            [datascript.core :as d]))

(enable-console-print!)

;; ======================================================================
;; Constants

(def grid-size [600 600])

(def frame-rate 20)

;; ======================================================================
;; Squares

(defn rand-rgb []
  {:r (rand-int 255) :g (rand-int 255) :b (rand-int 255)})

(defn sq->rgb [sq]
  [(:r sq) (:g sq) (:b sq)])

;; TODO: take x as arg to decouple from grid-size
(defn rand-square []
  (let [side 40]
    (merge {:side side :y 0 :x (rand-int (- (first grid-size) side))}
           (rand-rgb))))

(defn overlap?
  "Checks if two integer intervals overlap"
  [[a-min a-max] [b-min b-max]]
  (and (< a-min b-max) (< b-min a-max)))

(defn sq->interval
  "Gets the x interval the square occupies on the screen"
  [{:keys [x side]}]
  [x (+ x side)])

(defn sq-overlap?
  "Checks if two squares' x coordinates overlap"
  [a b]
  (overlap? (sq->interval a) (sq->interval b)))

(defn sq->top
  "Gets the y coordinate for the squares top side"
  [{:keys [y side]}]
  (+ y side))

(defn sq-clear? [db sq-id]
  (empty? (d/q '[:find ?supports :in $ ?id
                 :where [?id :supports ?supports]]
               db
               sq-id)))

(defn dist [a b]
  (letfn [(d [k]
            (Math/pow (- (get a k) (get b k)) 2))]
    (Math/sqrt (+ (d :x) (d :y)))))

;; http://clj-me.blogspot.com.uy/2009/06/linear-interpolation-and-sorted-map.html
(defn interpolator
  "Takes a coll of 2D points (vectors) and returns
  their linear interpolation function."
  [points]
  (let [m (into (sorted-map) points)]
    (fn [x]
      (assert (number? x))
      (let [[[x1 y1]] (rsubseq m <= x)
            [[x2 y2]] (subseq m > x)]
        (let [out (if x2
                    (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1))))
                    y1)]
          (assert (not (js/isNaN out)) [x1 x2 y1 y2 x])
          out)))))

(defn path-through
  "Returns a path through all the sqs"
  [& sqs]
  (let [d (map dist (drop 1 sqs) sqs)
        total-d (apply + d)
        w (map #(/ % total-d) d)
        inc-w (reduce (fn [acc v] (conj acc (+ v (last acc)))) [0] w)]
    (juxt (interpolator (map vector inc-w (map :x sqs)))
          (interpolator (map vector inc-w (map :y sqs))))))

(defn rect-path
  "Returns an arc-like path to the target"
  [sq target-sq]
  (let [sq-i (if (< (:y sq) (:y target-sq))
               (assoc sq :y (+ (:side target-sq) (:y target-sq)))
               (update sq :y (partial + (:side sq))))]
    (path-through sq sq-i (assoc sq-i :x (:x target-sq)) target-sq)))

(defn sq->center
  [{:keys [x y side]}]
  [(+ x (/ side 4)) (+ y (/ side 1.5))])

(defn ent->map [ent]
  (merge {:db/id (:db/id ent)} ent))

(defn get-sq [db id]
  {:pre [(number? id)]}
  (ent->map (d/entity db id)))

(defn db->squares [db]
  (->> (d/q '[:find ?id :where [?id :x _]] db)
       (map first)
       (map (partial get-sq db))))

(defn find-support
  "Returns the support for an x position, or nil if none is found"
  [conn sq]
  (some->> (db->squares @conn)
           (filter (partial sq-overlap? sq))
           (apply max-key sq->top)))

(defn stack-tx [sq target-sq]
  [[:db/add (:db/id target-sq) :supports (:db/id sq)]
   [:db/add (:db/id sq) :y (sq->top target-sq)]
   [:db/add (:db/id sq) :x (:x target-sq)]])

(defn op->tx [db {:keys [move to type]}]
  (if (= :claw type)
    []
    (concat (mapv (fn [[id]] [:db/retract id :supports move])
                  (d/q '[:find ?id :in $ ?sq
                         :where [?id :supports ?sq]]
                       db
                       move))
            (stack-tx (get-sq db move) (get-sq db to)))))

(defn stack-sq!
  "Stacks the new sq on top of the squares"
  [conn sq]
  (d/transact conn
              (let [temp-id -1
                    sq' (assoc sq :db/id temp-id)]
                (concat [sq']
                        (if-let [support-sq (find-support conn sq)]
                          (stack-tx sq' support-sq)
                          [[:db/add temp-id :y 0]])))))

(defn stack-squares!
  "Add n stacked squares to conn"
  [conn n]
  (doseq [sq (map (fn [_] (rand-square)) (range n))]
    (stack-sq! conn sq)))

(defn on-top?
  "Is a on top of b?"
  [a b]
  (and (sq-overlap? a b)
       (= (:y a) (sq->top b))))

(defn supported-by
  "Returns all the squares that support sq, where y-sqs is indexed by sq->top"
  [y-sqs sq]
  (some->> (get y-sqs (:y sq))
           (filter #(and (not= sq %) (on-top? sq %)))))

(defn support-pairs
  "Returns all the support pairs in the indexed stacked-squares"
  [y-sqs]
  (->> (vals y-sqs)
       (mapcat identity)
       (mapcat (fn [sq]
                 (map #(vector (:db/id sq) (:db/id %)) (supported-by y-sqs sq))))))

(defn clear-sqs
  "Returns all the sqs which are not supporting other sqs"
  [sqs]
  (->> sqs
       (filter (fn [sq] (every? #(not (on-top? % sq)) sqs)))
       (map :db/id)))

(defn possible-actions
  "All the possible actions for a determined state"
  [sqs]
  (let [c-sqs (clear-sqs sqs)]
    (->> (for [x c-sqs y c-sqs]
           (when-not (= x y) [x y]))
         (remove nil?)
         (map (fn [[x y]] {:type :move :move x :to y})))))

(defn find-sq [sqs id]
  (first (filter #(= id (:db/id %)) sqs)))

;; goal is a over b  as [a b]
(defn distance [goal sqs]
  (+ (count (supported-by sqs (find-sq sqs (first goal))))
     (count (supported-by sqs (find-sq sqs (second goal))))))

(defn valid-op? [db {:keys [type move to] :as op}]
  (and (some? (d/entity db move)) (some? (d/entity db to))
       (or (not= :move type)
           (and (sq-clear? db move) (sq-clear? db to)))))

(defn apply-op!
  "Returns the squares after the operation is applied"
  [conn {:keys [move to] :as op}]
  (d/transact conn (op->tx @conn op)))

(defn step-op [db op]
  (d/db-with db (op->tx db op)))

(defn valid-plan?
  [db plan]
  (->> plan
       (reduce (fn [db op]
                 (when (and (some? db) (valid-op? db op))
                   (step-op db op)))
               db)
       some?))

;; ======================================================================
;; Quil

(defn clear-canvas! []
  (q/fill 192 192 192)
  (apply q/rect 0 0 grid-size))

(defn xy->xy [sq]
  (update sq :y #(- (first grid-size) (:side sq) %)))

(defn sq-text! [sq]
  (q/stroke 0 0 0)
  (q/fill 0 0 0)
  (let [[x y] (sq->center sq)]
    (q/text (:db/id sq) x y)))

(defn square!
  "Draws a square in the screen"
  [sq]
  (let [{:keys [x y side] :as sq'} (xy->xy sq)]
    (apply q/fill (sq->rgb sq'))
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

(defonce app-state
  (atom {:conn nil
         :ops []
         :frame 0}))

(defn state->render
  "Data to render"
  [db op f]
  (if (empty? op)
    {:squares (db->squares db)}
    (let [sq-id (:move op)
          sq (get-sq db sq-id)
          target-sq (get-sq db (:to op))
          target-sq' (assoc target-sq :y (sq->top target-sq))
          [x y] ((rect-path sq target-sq') f)
          sq' (assoc sq :x x :y y)]
      {:claw sq'
       :squares (db->squares (if (= :move (:type op))
                               (d/db-with db [[:db/add sq-id :x x]
                                              [:db/add sq-id :y y]])
                               db))})))

(defn add-claw-moves
  "Add intermediate claw moves to the plan (for rendering)"
  [ops]
  (->> (drop 1 (cycle ops))
       (map (fn [{:keys [to]} {:keys [move]}]
              {:type :claw :move to :to move})
            ops)
       (interleave ops)
       vec))

(defn setup []
  (q/frame-rate frame-rate)
  (q/color-mode :rgb)
  (q/background 200)
  (let [schema {:supports {:db/cardinality :db.cardinality/many
                           :db/valueType :db.type/ref}}
        conn (d/create-conn schema)]
    (stack-squares! conn 10)
    (swap! app-state
           #(assoc %
                   :conn conn :frame 0
                   :ops (let [plan [{:type :move :move 8 :to 10}
                                    {:type :move :move 8 :to 9}]]
                          (if (valid-plan? @conn plan)
                            (cycle (add-claw-moves plan))
                            []))))))

(defn draw []
  (clear-canvas!)
  (let [{:keys [conn ops frame]} @app-state
        op (first ops)
        {:keys [squares claw]} (state->render @conn op frame)]
    (doseq [sq squares]
      (square! sq))
    (when (and (:x claw) (:y claw))
      (grip! claw))
    (swap! app-state
           (fn [s]
             (let [f (:frame s)]
               (if (> 1 f)
                 (assoc s :frame (+ f (/ 1 frame-rate)))
                 (do (when (some? op)
                       (apply-op! conn op))
                     (assoc s :frame 0 :ops (rest (:ops s))))))))))

(q/defsketch example
  :title "Oh so many grey circles"
  :host "canvas"
  :settings #(q/smooth 2) ;; Turn on anti-aliasing
  :setup setup
  :draw draw
  :size grid-size)

;; ======================================================================
;; DOM Setup

(defn widget [data owner]
  (reify
    om/IRender
    (render [this]
      (dom/div nil ""))))

(defn init []
  (om/root widget {:text "Hello world!"}
           {:target (. js/document (getElementById "container"))}))
