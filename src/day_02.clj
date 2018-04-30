(ns day-02
  (:require utils
            [clojure.string :as str]
            [inputs :refer [day-02] :rename {day-02 input}]))

(defn- str->row [row]
  (->> (str/split row #"\s+")
       (filter (complement empty?))
       (map #(Integer/parseInt %))))

(defn- str->spreadsheet [input]
  (let [str-rows (str/split-lines input)]
    (map str->row str-rows)))

(defn- check-bounds [candidate current-bound op]
  (if (or (nil? current-bound) (op candidate current-bound))
    candidate
    current-bound))

(defn- check-min [candidate min] (check-bounds candidate min <))

(defn- check-max [candidate max] (check-bounds candidate max >))

(defn row-checksum-min-max [spreadsheet-row]
  (loop [[first & rest] spreadsheet-row
         min nil
         max nil]
    (if (nil? first)
      (- max min)
      (recur rest (check-min first min) (check-max first max)))))

(defn- is-divisible? [number other-number]
  (= 0 (if (< number other-number)
         (mod other-number number)
         (mod number other-number))))

(defn- find-divisible-number
  [item others]
  (let [filter-fn (partial is-divisible? item)]
    (->> others
         (filter filter-fn)
         first)))

(defn row-checksum-divisible [spreadsheet-row]
  (println spreadsheet-row)
  (loop [[cur-cell & rest] spreadsheet-row
         dividend 0
         divisor 0]
    (cond
      (nil? cur-cell) (/ dividend divisor)
      :else (let [div-number (find-divisible-number cur-cell rest)]
              (if (nil? div-number)
                (recur rest dividend divisor)
                (recur nil (max cur-cell div-number) (min cur-cell div-number)))))))

(defn get-checksum [input]
  (let [spreadsheet (str->spreadsheet input)]
    (->> spreadsheet
         (map row-checksum-divisible)
         (reduce +))))

(get-checksum input)
