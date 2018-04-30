(ns day-07
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [utils :refer [get-symbol-list]]))
;;;; Specs
(s/def ::prog-name symbol?)

(s/def ::prog-weight (s/and (s/coll-of int)
                            #(= 1 (count %))))

(s/def ::supported-list (s/* ::prog-name))

(s/def ::supporting (s/? (s/cat :arrow #{'->}
                                :programs ::supported-list)))

(s/def ::program (s/cat :name ::prog-name
                        :weight ::prog-weight
                        :supporting ::supporting))

(defn parse-raw-entry
  [raw-entry]
  (let [{:keys [name weight supporting]} (s/conform ::program raw-entry)
        supported (:programs supporting)]
    (-> {:name (keyword name)
         :weight (first weight)}
        (conj (when supported [:supporting (map keyword supported)])))))

(defn add-tree-entry
  [tower-tree {:keys [name weight supporting] :or {supporting []}}]
  (let [entry (get tower-tree name)
        supp-entries (mapcat #(list % {:parent name}) supporting)
        tower-tree' (if (empty? supp-entries)
                      tower-tree
                      (apply assoc tower-tree supp-entries))]
    (if entry
      ;; existing entry, add details
      (assoc tower-tree' name (assoc entry
                                     :children supporting
                                     :weight weight))
      ;; New entry insert another for that
      (assoc tower-tree' name {:children supporting
                               :weight weight
                               :parent nil}))))


;; add-tree-entry example
#_(add-tree-entry {} {:name 'a :weight 22 :supporting '[b c]})
#_(add-tree-entry
   {:b {:parent :a, :supporting [], :weight nil},
    :a {:parent nil, :supporting [:b :c], :weight 22}}
   {:name :b :weight 12 :supporting [:c]})

;; create-tower as lazy-list example
#_(let [inputs [{:name :a :weight 2 :supporting [:b]}
              {:name :b :weight 2 :supporting [:c :d]}
              {:name :c :weight 2}
              {:name :d :weight 2}] last-tree (atom {})
      outputs (map (fn [{:keys [name weight supporting] :or {supporting []}}]
                     (let [current-entry [name {:weight weight :parent nil :children supporting}]
                           supp-entries (mapv #(hash-map % {:parent name}) supporting)]
                       into {} [current-entry supp-entries]))
                   inputs)]
  (into {} outputs))

(defn create-tower
  [inputs]
  (loop [[first & rest] inputs
         tree {}]
    (if first
      (recur rest (add-tree-entry tree first))
      tree)))

(defn get-base-program
  [tower-tree]
  (some->> tower-tree
           (filter #(nil? (:parent (% 1))))))

;; Find base element
(->> (slurp "src/input/day_07.txt")
   get-symbol-list
   (map parse-raw-entry)
   create-tower
   get-base-program)


(let [input "tree > squerrel, gecko, acorn" ; Sample dsl input
      input-object (->> input
                      (format "[%s]") ;; Make valid edn string
                      (clojure.edn/read-string))] ;; parse edn string
  (s/def ::something symbol?)
  (s/def ::connected-exp (s/cat :parent-thing ::something
                                :rel-marker #{'>}
                                :related (s/* ::something)))
  (s/conform ::connected-exp input-object))
;; => {:parent-thing tree, :child-marker >, :related [squerrel gecko acorn]}

(map :foo [{:foo 1} {:foo 2}])
