(ns aoclj-2018.day-03
  (:require [aoclj-2018.data.day-03 :refer [data-seq input]]))

;;data converter
(set! *warn-on-reflection* true)

(defn square-coords [{[x y] :coords
                      [w h] :dimensions
                      id :id}]
  (for [x (range x (+ x w))
        y (range y (+ y h))]
    [x y]))

(defn collides?
  [{[x0 y0] :coords [w0 h0] :dimensions}
   {[x1 y1] :coords [w1 h1] :dimensions}]
  (not (or (< (+ x1 (dec w1)) x0)
           (< (+ y1 (dec h1)) y0)
           (< (+ x0 (dec w0)) x1)
           (< (+ y0 (dec h0)) y1))))

(defn part-1
  [data]
  (->> data
       (map square-coords)
       (mapcat identity)
       frequencies
       vals
       (filter (partial < 1))
       count))

(defn part-2
  [data]
  (loop [[entry & others] data]
    (if entry
      (if (some (partial collides? entry) others)
        (recur (concat others (list entry)))
        entry))))

(time (part-1 (data-seq input)))

(time (part-2 (data-seq input)))

(comment
  (let [[a b c d :as data] (data-seq ["#1 @ 1,3: 4x4"
                                      "#2 @ 3,1: 4x4"
                                      "#3 @ 5,5: 2x2"])]
    #_(collides? c d)
    #_(some #((partial collides? a)) [a b c])
    (part-2 data))

  (let [col (take 3 (data-seq input))]
    (some (complement (partial collides? (first col))) (rest col))))
