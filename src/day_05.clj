(ns day-05
  (:require [utils :refer [clean-input]]
            [clojure.string :as str]))


(defn traverse-maze
  [input offset-fn]
  (loop [maze input
         steps-taken 0
         i 0]
    (if-let [current-val (get maze i)]
      (recur
       (assoc maze i (offset-fn current-val))
       (inc steps-taken)
       (+ i current-val))
      {:steps-taken steps-taken
       :final-maze maze})))

#_(clean-input test-input)
#_(traverse-maze [2 2 -1 -2 0])

;; Solution
(-> (slurp "src/input/day_05.txt")
    clean-input
    (traverse-maze inc) ; part 1
    #_(traverse-maze #(if (>= % 3)
                        (dec %)
                        (inc %))) ; part 2
    :steps-taken)
