(ns day-03
  (:require
   [clojure.string :as str]
   [clojure.set :as cset]
   [inputs :refer [day-03] :rename {day-03 input}]))

(def base-adjacent-nums 8)

(defn level-count
  "Returns the number of items for a `level`"
  [level]
  (+ base-adjacent-nums (* level 8)))

(defn level-start-num
  "Returns the first number for a given `level`"
  [level]
  (loop [cur-level level
         count 0]
    (cond
      (= cur-level 0) (+ 2 count)
      :else (let [prev-level-count (level-count (dec cur-level))]
              (recur (dec cur-level) (+ count prev-level-count))))))

(defn get-level
  "Get level info for a number.
  Given a `number`, return map containing the level the number appears,
  the starting number for that level, and the index of the number in the level."
  [number]
  (let [start-seq (map level-start-num (iterate inc 0))
        found-seq (take-while #(<= % number) start-seq)
        start (last found-seq)]
    {:level (dec (count found-seq))
     :start start
     :offset (- number start)}))

;; getting a swinging sequence
(def opts #{inc dec})
(defn- next-num
  [start end]
  (let [op (atom dec)]
    (fn [item]
      (let [next (@op item)]
        (if (<= start next end) next
            (do (swap! op #(first (cset/difference opts #{%})))
                (@op item)))))))

(defn- frequency-sequence
  ([start end] (frequency-sequence start end start))
  ([start end initial]
   (iterate (next-num start end) initial)))

(defn- moves-range
  [level]
  [(inc level) (->> level inc (* 2))])

(defn steps-to-origin
  "Give a num, return how many steps to return to 1 in spiral sequence"
  [num]
  (cond
    (= 1 num) 0
    (> num 1) (let [{:keys [level offset]} (get-level num)
                    [min max] (moves-range level)
                    step-sequence (frequency-sequence min max (dec max))]
                (nth step-sequence offset))))

(steps-to-origin input)
(time (steps-to-origin input))

;; Part 2
(defn- offset-coord
  "When supplied with a coordinate of form [x y] and an offset [xd yd]
  returns new coordinate with each element with offset added to it."
  [[coord-x coord-y] [offset-x offset-y]]
  [(+ coord-x offset-x) (+ coord-y offset-y)])

(def neighbor-offsets [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])
(defn- add-surrounding
  "For a given coordinate and grid, sums all elements surrounding it.
  nils are counted as 0."
  [coord grid]
  (reduce
   (fn [sum offset]
     (+ sum (if-let [val (grid (offset-coord coord offset))]
              val
              0)))
   0
   neighbor-offsets))

#_(add-surrounding [0 1] {[0 0] 1
                          [1 0] 1
                          [1 1] 2})

(def initial-offsets [[1 0] [0 1] [-1 0] [0 -1]])

(defn abs [x] (max x (- x)))

(defn- next-step-details
  "Get next direction offset, if it is past the level boundary
  then use the next offset if possible and return that.
  If no offsets are left, then end has been reached and start with
  next offset order"
  [{:keys [coord offsets turns-made boundary] :as step-details}]
  (let [[cur-offset & rest-offsets :as offsets] offsets
        next-coord (offset-coord coord cur-offset)]
    (if-let [past-boundary? (some #(> (abs %) boundary) next-coord)]
      ;; append offset to end of vector and recalc coordinate.
      (let [next-offsets (conj (vec rest-offsets) cur-offset)
            next-coord (offset-coord coord (first next-offsets))
            next-turns-made (mod (inc turns-made) 4)
            next-boundary (if (= next-turns-made 0) (inc boundary) boundary)]
        (assoc step-details
               :coord next-coord
               :offsets next-offsets
               :turns-made next-turns-made
               :boundary next-boundary))
      (assoc step-details
             :coord next-coord
             :offsets offsets
             :turns-made turns-made))))

#_(next-step-details {:coord [1 0]
                      :offsets initial-offsets
                      :turns-made 0
                      :boundary 1})

(defn walk-grid
  [n]
  (loop [grid {[0 0] 1} ;starting grid to use
         step-details {:coord [1 0]
                       :offsets initial-offsets
                       :boundary 1
                       :turns-made 0}
         items [1]
         idx 0]
    (if (= idx n) items
        (let [cur-coord (:coord step-details)
              neighbor-sum (add-surrounding cur-coord grid) ; Get sum of neighbors
              next-grid (assoc grid cur-coord neighbor-sum)
              next-details (next-step-details step-details)] ;update grid
          (println next-details)
          (recur next-grid
                 next-details
                 (conj items neighbor-sum)
                 (inc idx))))))

(walk-grid 100) ;inspect to find number, find abetter way to do it, maybe by iterate to query it
;; TODO create iteratable version
