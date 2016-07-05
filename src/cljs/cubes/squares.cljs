(ns cubes.squares
  (:require [clojure.set :as set]
            [datascript.core :as d]))

(defn screen-size [db]
  (first (d/q '[:find ?w ?h
                :where [?s :screen-width ?w] [?s :screen-height ?h]]
              db)))

(defn rand-rgb []
  [(rand-int 255) (rand-int 255) (rand-int 255)])

(defn rand-square [max-x]
  (let [side 40]
    {:side side
     :y 0 :x (rand-int (- max-x side))
     :rgb (rand-rgb)}))

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

(defn sq-clear?
  "Checks if the sq is clear (if it's not supporting any other squares)"
  [db sq-id]
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

(defn db->squares
  "Coll with all the squares in db"
  [db]
  (->> (d/q '[:find ?id :where [?id :x _]] db)
       (map first)
       (map (partial get-sq db))))

(defn find-support
  "Returns the support for an x position, or nil if none is found"
  [db sq]
  (some->> (db->squares db)
           (filter (partial sq-overlap? sq))
           (apply max-key sq->top)))

(def on-top '[[[on-top ?a ?b]
               [?b :supports ?a]]
              [[on-top ?a ?b]
               [?b :supports ?x]
               [on-top ?a ?x]]])

(defn sq-supports
  "All squares a particular square supports"
  [db sq]
  (letfn [(y [sq]
            (ffirst (d/q '[:find ?y :in $ ?sq :where [?sq :y ?y]] db sq)))]
    (->> (d/q '[:find ?id :in $ % ?sq
                :where [on-top ?id ?sq]]
              db on-top sq)
         (map first)
         (sort-by y))))

(defn supported-by
  "All the squares that support sq, where y-sqs is indexed by sq->top"
  [db sq-id]
  (->> sq-id
       (d/q '[:find ?id :in $ ?sq :where [?id :supports ?sq]] db)
       (map first)))

(defn clear-sqs
  "All the sqs which are not supporting other sqs"
  [db]
  (let [all-sqs (set (map :db/id (db->squares db)))
        support-sqs (->> db
                         (d/q '[:find ?id :where [?id :supports _]])
                         (map first)
                         set)]
    (set/difference all-sqs support-sqs)))

(defn stack-tx
  "Tx data to stack sq on top of target-sq"
  [sq target-sq]
  [[:db/add (:db/id target-sq) :supports (:db/id sq)]
   [:db/add (:db/id sq) :y (sq->top target-sq)]
   [:db/add (:db/id sq) :x (:x target-sq)]])

(defn unsupport-tx
  "Tx data to clear sq from all of its supports"
  [db sq]
  (mapv (fn [id] [:db/retract id :supports sq])
        (supported-by db sq)))

(defmulti op->tx
  "Transforms an operation into the necessary tx data to be applied to db"
  (fn [_ op] (:type op)))

(defmethod op->tx :default [_ _] [])

(defmethod op->tx :move
  [db {:keys [move to]}]
  (concat (unsupport-tx db move)
          (stack-tx (get-sq db move) (get-sq db to))))

(defn find-clear-space
  "Coordinate that has an adjacent clear space (length: width)"
  [db width]
  {:post [(some? %)]}
  (let [screen-width (first (screen-size db))
        base-sqs (->> db
                      (d/q '[:find (pull ?i [:db/id :side :x])
                             :where [?i :y 0]])
                      (mapcat identity))]
    (->> base-sqs
         (map #(+ (:side %) (:x %)))
         (concat [0])
         (filter (fn [x]
                   (and (< x screen-width)
                        (every? #(not (sq-overlap? {:x x :side width} %))
                                base-sqs))))
         first)))

(defmethod op->tx :get-rid-of
  [db {:keys [move]}]
  (let [sq (get-sq db move)]
    (if (zero? (:y sq))
      []
      (concat (unsupport-tx db move)
              [[:db/add move :y 0]
               [:db/add move :x (find-clear-space db (:side sq))]]))))

(defn apply-op!
  "Returns the squares after the operation is applied"
  [conn {:keys [move to] :as op}]
  (d/transact conn (op->tx @conn op)))

(defn step-op
  "Returns a new database as if the operation was applied"
  [db op]
  (d/db-with db (op->tx db op)))

(defn stack-sq
  "Stacks the new sq on top of the squares"
  [db sq]
  (d/db-with db (let [temp-id -1
                      sq' (assoc sq :db/id temp-id)]
                  (concat [sq']
                          (if-let [support-sq (find-support db sq)]
                            (stack-tx sq' support-sq)
                            [[:db/add temp-id :y 0]])))))

(defn stack-squares
  "Add n stacked squares to db"
  [db n]
  (loop [db db
         sqs (let [screen-width (first (screen-size db))]
               (map (fn [_] (rand-square screen-width)) (range n)))]
    (if-let [sq (first sqs)]
      (recur (stack-sq db sq) (rest sqs))
      db)))

(defmulti valid-op? (fn [_ op] (:type op)))

(defmethod valid-op? :default [_ _] true)

(defn sq-exist? [db id]
  (some? (d/entity db id)))

(defn sqs-exist? [db {:keys [move to]}]
  (and (sq-exist? db move)  (sq-exist? db to)))

(defmethod valid-op? :get-rid-of [db op]
  (let [sq (get-sq db (:move op))]
    (and (some? sq) (some? (find-clear-space db (:side sq))))))

(defmethod valid-op? :claw [db op] (sqs-exist? db op))

(defmethod valid-op? :move [db {:keys [move to] :as op}]
  (and (sqs-exist? db op) (sq-clear? db move) (sq-clear? db to)))

(defmethod valid-op? :put [db op] (sqs-exist? db op))

(defn coords->sq
  "Returns the square the coordinates belong to (if any)"
  [db [x y]]
  {:pre [(d/db? db)]}
  (let [between? '[[[between ?x0 ?side ?x]
                    [(< ?x0 ?x)]
                    [(+ ?x0 ?side) ?x1]
                    [(< ?x ?x1)]]]
        in-sq? '[[[in-sq ?x ?y ?id]
                  [?id :x ?x0]
                  [?id :y ?y0]
                  [?id :side ?side]
                  [between ?x0 ?side ?x]
                  [between ?y0 ?side ?y]]]]
    (ffirst (d/q '[:find ?sq :in $ % ?x ?y
                   :where [in-sq ?x ?y ?sq]]
                 db (concat between? in-sq?) x y))))

;; ======================================================================
;; Planning

(defn maybe-step-op
  "Step-op that checks if the op is valid, returns nil if not"
  [db op]
  (when (and (some? db) (valid-op? db op))
    (step-op db op)))

(defn valid-plan?
  "Checks if the plan is valid by applying all the ops"
  [db plan]
  (some? (reduce maybe-step-op db plan)))

(defn done?
  "Is the goal achieved in the db?"
  [[sq tsq] db]
  (contains? (d/q '[:find ?s :in $ ?tsq
                    :where [?tsq :supports ?s]]
                  db tsq)
             [sq]))

(defn possible-ops
  "All possible future ops for a determined db"
  [db]
  (let [c-sqs (clear-sqs db)]
    (->> (for [x c-sqs y c-sqs]
           (when-not (= x y) [x y]))
         (remove nil?)
         (map (fn [[x y]] {:type :move :move x :to y}))
         (concat (map (fn [sq] {:type :get-rid-of :move sq}) c-sqs)))))

(defn distance [[sq tsq] db]
  (if (done? [sq tsq] db)
    (- js/Infinity)
    (+ (count (sq-supports db sq)) (count (sq-supports db tsq)))))

(defn plan-moves [goal db]
  (loop [db db
         plan []
         n 0]
    (cond
      (done? goal db) plan
      (= 100 n) [:not-found plan]
      :else (let [ops (->> (possible-ops db)
                           (map (juxt identity (partial step-op db)))
                           (sort-by (comp (partial distance goal) second)))]
              (let [[op db'] (first ops)]
                (recur db' (conj plan op) (inc n)))))))

;; ======================================================================
;; Advanced planning

;; TODO: define the correct recursive structure of an expanded tree

;; TODO: get-rid-of could be implemented in terms of move
(def base-ops #{:move :claw :get-rid-of})

(defmulti expand-op (fn [_ op] (:type op)))

(defmethod expand-op :default [_ op] [op])

(defmethod expand-op :put [_ {:keys [to move]}]
  [{:type :find-space :sq to}
   {:type :grasp :move move}
   {:type :move :move move :to to}])

(defmethod expand-op :find-space [_ {:keys [sq]}]
  [{:type :clear-top :sq sq}])

(defmethod expand-op :grasp [_ {:keys [move]}]
  [{:type :clear-top :sq move}])

(defmethod expand-op :clear-top [db {:keys [sq]}]
  (let [sqs (sq-supports db sq)]
    (mapv (fn [sq] {:type :get-rid-of :move sq}) (reverse sqs))))

(defn base-op? [op]
  (contains? base-ops (:type op)))

(defn base-ops? [ops]
  (every? base-op? ops))

;; FIX: the ops are all expanded with the same db
;; while each should be expanded on a db with all the changes from
;; previous changes.
;; -> use reduce sequentially
(defn expand-ops [db ops]
  (loop [ops ops]
    (if (base-ops? ops)
      ops
      (recur (mapcat (partial expand-op db) ops)))))

(defn expand-tree* [db [op ops]]
  (cond
    (base-ops? ops) [op ops]
    :else [op (mapv #(if (base-op? %)
                       [%]
                       (expand-tree* db [% (expand-op db %)]))
                    ops)]))

(defn expand-tree [db op]
  (expand-tree* db [op (expand-op db op)]))

;; ======================================================================
;; Language

(defmulti op->sentence :type)

(defmethod op->sentence :get-rid-of [{:keys [move]}]
  (str "Get rid of " move))

(defmethod op->sentence :put [{:keys [move to]}]
  (str "Put " move " on " to))

(defmethod op->sentence :move [{:keys [move to]}]
  (str "Move " move " to " to))

(defmethod op->sentence :claw [{:keys [move to]}]
  (str "Move the claw from " move " to " to))

(defmethod op->sentence :grasp [{:keys [move]}]
  (str "Grasp " move))

(defmethod op->sentence :clear-top [{:keys [sq]}]
  (str "Clear " sq "'s top"))

(defmethod op->sentence :find-space [{:keys [sq]}]
  (str "Find space on " sq))
