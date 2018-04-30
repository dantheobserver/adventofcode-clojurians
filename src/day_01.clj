(ns day-01
  (:require [utils]))

(defn- char->int [chr] (Integer/parseInt (str chr)))

(defn- str->int-seq [str-seq] (map char->int str-seq))

(defn match-next-seq
  ([int-seq] (match-next-seq int-seq 1))
  ([int-seq step]
   (let [seq-vec (into [] int-seq)
         list-size (count seq-vec)]
     (map-indexed
      (fn [idx itm]
        (let [next-idx (-> idx
                           (+ step)
                           (mod list-size))
              next-itm (nth seq-vec next-idx)]
          (if (= itm next-itm) itm 0)))
      seq-vec))))

(defn match-half-step-seq
  [int-seq]
  (match-next-seq int-seq (-> int-seq count (/ 2))))

(defn seq-sum
  [seq-fn string-seq]
  (->> string-seq
       str->int-seq
       seq-fn
       (reduce +)))

;; Print cases
(utils/print-expr (seq-sum match-next-seq "1122"))
(utils/print-expr (seq-sum match-next-seq "1111"))
(utils/print-expr (seq-sum match-next-seq "1234"))
(utils/print-expr (seq-sum match-next-seq "91212129"))

;; Half-step solution
(utils/print-expr (seq-sum match-half-step-seq "1212" ))
(utils/print-expr (seq-sum match-half-step-seq "1221"))
(utils/print-expr (seq-sum match-half-step-seq "123425"))
(utils/print-expr (seq-sum match-half-step-seq "123123"))
(utils/print-expr (seq-sum match-half-step-seq "12131415"))
