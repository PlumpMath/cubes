(ns cubes.app
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [quil.core :as q :include-macros true]))

;; ======================================================================
;; Constants

(def grid-size [600 600])

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

(enable-console-print!)

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
      (let [[[x1 y1]] (rsubseq m <= x)
            [[x2 y2]] (subseq m > x)]
        (if x2
          (+ y1 (* (- x x1) (/ (- y2 y1) (- x2 x1))))
          y1)))))

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
  (let [sq-i (update sq :y (partial + (:side sq)))]
    (path-through sq sq-i
                  (assoc sq-i :x (:x target-sq))
                  (assoc target-sq :y (sq->top target-sq)))))

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

(defn idx-squares [sqs]
  (zipmap (map :id sqs) sqs))

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
                 (map #(vector (:id sq) (:id %)) (supported-by y-sqs sq))))))

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
         :frame 0}))

(comment
  (println (sort-by first (support-pairs (group-by sq->top (vals sqs))))))

(defn setup []
  (q/frame-rate 20)
  (q/color-mode :rgb)
  (q/background 200)
  (swap! app-state assoc :squares (sort-by :y (stacked-squares 50))))

(defn draw []
  (clear-canvas!)
  (let [sqs (:squares @app-state)]
    (doseq [sq (drop-last 1 sqs)]
      (square! sq))
    (let [[target-sq sq] (take-last 2 sqs)
          [x y] ((rect-path sq target-sq) (:frame @app-state))
          sq' (assoc sq :x x :y y)]
      (square! sq')
      (grip! sq')
      (swap! app-state update :frame #(if (> 1 %) (+ % (/ 1 20)) 0)))))

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
