(ns cubes.app
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [quil.core :as q :include-macros true]))

;; ======================================================================
;; Constants

(def grid-size [600 600])

;; ======================================================================
;; Squares

;; TODO: use another rand fn to avoid q/

(defn rand-rgb []
  [(q/random 255) (q/random 255) (q/random 255)])

;; TODO: take x as arg to decouple from grid-size
(defn rand-square []
  (let [side 20]
    {:id (gensym) :rgb (rand-rgb) :side side
     :y 0 :x (q/random (- (first grid-size) side))}))

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

(defn posible-y
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
  (conj squares (assoc sq :y (posible-y squares sq))))

(defn stacked-squares
  "Generate n stacked-squares"
  [n]
  (loop [sqs (map (fn [_] (rand-square)) (range n))
         stack []]
    (if (seq sqs)
      (recur (rest sqs) (stack-sq stack (first sqs)))
      stack)))

(defn on-top?
  "Is a on top of b?"
  [a b]
  (and (sq-overlap? a b)
       (= (:y a) (sq->top b))))

;; ======================================================================
;; Quil

(defn clear-canvas! []
  (q/fill 192 192 192)
  (apply q/rect 0 0 grid-size))

(defn setup []
  (q/frame-rate 1)
  (q/color-mode :rgb)
  (q/background 200))

(defn xy->xy [sq]
  (update sq :y #(- (first grid-size) (:side sq) %)))

(defn square!
  "Draws a square in the screen"
  [sq]
  (let [{:keys [rgb x y]} (xy->xy sq)]
    (apply q/fill rgb)
    (let [s (:side sq)]
      (q/rect x y s s))))

(def done? (atom false))

(defn draw []
  (when-not @done?
    (clear-canvas!)
    (doseq [sq (stacked-squares 100)]
      (square! sq))
    (reset! done? true)))

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
      (dom/h1 nil "asd"))))

(defn init []
  (om/root widget {:text "Hello world!"}
           {:target (. js/document (getElementById "container"))}))
