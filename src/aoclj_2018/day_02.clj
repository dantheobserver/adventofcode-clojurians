(ns aoclj-2018.day-02
  (:require [clojure.test :refer [testing is]]))

(defn box-counts
  [counts]
  (->> counts
       vals
       (filter (fn [x] (or (= x 2)
                           (= x 3))))
       (group-by identity)))

(defn twos-threes [counts]
  (let [lookup (box-counts counts)]
    [(if (get lookup 2) 1 0)
     (if (get lookup 3) 1 0)]))

;; (box-counts {\a 2 \b 3 \c 3})
(defn checksum [input]
  (loop [[c & chars] input
         t-2 0
         t-3 0
         counts {}]
    (if c
      (if (= c \newline)
        (let [[twos threes] (twos-threes counts)]
          (recur chars (+ t-2 twos) (+ t-3 threes) {}))
        (if (get counts c)
          (recur chars t-2 t-3 (update counts c inc))
          (recur chars t-2 t-3 (assoc counts c 1))))
      (* t-2 t-3))))

#_(let [input (slurp "resources/2018_02.txt")]
    (checksum input))

;; receive "iafkpfaw"
;; return  "*af**fa*"
(defn norm-str [input]
  (let [lone-chars (->> input
                        (group-by identity)
                        vals
                        (filter #(= (count %) 1))
                        flatten
                        (apply str))
        ]
    (if (empty? lone-chars)
      input
      (let [pattern (re-pattern (str "[" lone-chars "]"))]
        (clojure.string/replace input pattern "*")))))

(defn possible-matches [input]
  (->> input
       (re-seq #"\*")
       count
       (= 1)))

#_(possible-matches)
#_(def test-in )

(defn str-tester
  [in]
  (fn [input]
    (->> (interleave in input)
         (partition 2)
         (map #(into #{} %))
         (map count)
         (group-by #(when (< 1 %) :mismatch))
         :mismatch
         count
         (= 1))))

(time
 (loop [[in & strings] (clojure.string/split (slurp "resources/2018_02.txt") #"\n")]
   (if in
     (let [matches (filter (str-tester in) strings)]
       (if (seq matches)
         [assoc matches in]
         (recur strings))))))
