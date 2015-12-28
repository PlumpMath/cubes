(ns cubes.app
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [quil.core :as q :include-macros true]))

(enable-console-print!)

;; ======================================================================
;; Constants

(def grid-size [600 600])

(def frame-rate 20)

;; ======================================================================
;; Squares

(def c (atom 0))

(defn inc-id []
  (let [out @c]
    (swap! c inc)
    out))

(defn rand-rgb []
  [(rand-int 255) (rand-int 255) (rand-int 255)])

;; TODO: take x as arg to decouple from grid-size
(defn rand-square []
  (let [side 40]
    {:id (inc-id) :rgb (rand-rgb) :side side
     :y 0 :x (rand-int (- (first grid-size) side))}))

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

(defn sq-clear? [{:keys [squares]} sq-id]
  (let [sq (get squares sq-id)]
    (every? (complement (partial sq-overlap? sq))
            (get (group-by :y (vals squares)) (sq->top sq)))))

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
          (assert (not (js/isNaN out))
                  [x1 x2 y1 y2 x])
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

(defn possible-y
  "Returns y position for sq to be stacked on top of squares"
  [squares sq]
  (or (some->> squares
               (filter (partial sq-overlap? sq))
               (apply max-key sq->top)
               sq->top)
      0))

(defn stack-sq
  "Stacks the new sq on top of the squares"
  [squares sq]
  (conj squares (assoc sq :y (possible-y squares sq))))

(defn stacked-squares
  "Generate n stacked-squares"
  [n]
  (loop [sqs (map (fn [_] (rand-square)) (range n))
         stack []]
    (if (seq sqs)
      (recur (rest sqs) (stack-sq stack (first sqs)))
      stack)))

(defn idx-squares
  "Returns the squares idx by id"
  [sqs]
  (zipmap (map :id sqs) sqs))

(defn on-top?
  "Is a on top of b?"
  [a b]
  (and (sq-overlap? a b)
       (= (:y a) (sq->top b))))

;; TODO: this function requires a different index to be efficient
;; multiple indexes are necessary -> datascript
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
                 (map #(vector (:id sq) (:id %)) (supported-by y-sqs sq))))))

(defn clear-sqs
  "Returns all the sqs which are not supporting other sqs"
  [sqs]
  (->> sqs
       (filter (fn [sq] (every? #(not (on-top? % sq)) sqs)))
       (map :id)))

(defn possible-actions
  "All the possible actions for a determined state"
  [sqs]
  (let [c-sqs (clear-sqs sqs)]
    (->> (for [x c-sqs y c-sqs]
           (when-not (= x y) [x y]))
         (remove nil?)
         (map (fn [[x y]] {:type :move :move x :to y})))))

(defn find-sq [sqs id]
  (first (filter #(= id (:id %)) sqs)))

;; goal is a over b  as [a b]
(defn distance [goal sqs]
  (println (supported-by sqs (find-sq sqs (first goal))))
  (+ (count (supported-by sqs (find-sq sqs (first goal))))
     (count (supported-by sqs (find-sq sqs (second goal))))))

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
    (q/text (:id sq) x y)))

(defn square!
  "Draws a square in the screen"
  [sq]
  (let [{:keys [rgb x y side] :as sq'} (xy->xy sq)]
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

(defonce app-state
  (atom {:squares {}
         :claw {}
         :ops []
         :n 0
         :frame 0}))

(comment
  (println (sort-by first (support-pairs (group-by sq->top (vals sqs))))))

(defn valid-op? [s {:keys [type move to] :as op}]
  (println op)
  (println (sq-clear? s move) (sq-clear? s to))
  (and (contains? (:squares s) move)
       (contains? (:squares s) to)
       (or (not= :move type)
           (and (sq-clear? s move) (sq-clear? s to)))))

(defn apply-op
  "Returns the squares after the operation is applied"
  [s {:keys [move to] :as op}]
  (let [sq (get (:squares s) move)
        target-sq (get (:squares s) to)
        target-sq' (assoc target-sq :y (sq->top target-sq))
        [x y] ((rect-path sq target-sq') (:frame s))
        sq' (assoc sq :x x :y y)]
    (cond-> (assoc s :claw sq')
      (= :move (:type op)) (assoc-in [:squares (:id sq')] sq'))))

(defn valid-plan? [s ops]
  (:valid? (reduce (fn [acc op]
                     (cond
                       (not (:valid? acc)) acc
                       (not (valid-op? (:s acc) op))
                       (assoc acc :valid? false)
                       :else (update acc :s #(apply-op % op))))
                   {:valid? true :s (assoc s :frame 1)}
                   ops)))

(defn plan-moves [s ops]
  (if (valid-plan? s ops)
    (->> (drop 1 (cycle ops))
         (map (fn [{:keys [to]} {:keys [move]}]
                {:type :claw :move to :to move})
              ops)
         (interleave ops)
         vec)
    []))

(defn setup []
  (q/frame-rate frame-rate)
  (q/color-mode :rgb)
  (q/background 200)
  (swap! app-state
         #(let [sqs (sort-by :y (stacked-squares 10))
                idx (idx-squares sqs)]
            (assoc %
                   :squares idx
                   :ops (cycle (plan-moves {:squares idx}
                                           [{:type :move :move 7 :to 9}
                                            {:type :move :move 7 :to 8}]))))))


(defn draw []
  (clear-canvas!)
  (let [s @app-state
        op (first (:ops s))
        s' (apply-op s op)]
    (doseq [sq (vals (:squares s'))]
      (square! sq))
    (when (and (some? (get-in s' [:claw :x]))
               (some? (get-in s' [:claw :y])))
      (grip! (get s' :claw)))
    (swap! app-state
           (fn [s]
             (let [f (:frame s)]
               (if (> 1 f)
                 (assoc s :frame (+ f (/ 1 frame-rate)))
                 (assoc s' :frame 0 :ops (rest (:ops s')))))))))

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
