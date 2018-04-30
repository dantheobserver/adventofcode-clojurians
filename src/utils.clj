(ns utils
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn format-obj [expr] {:expr `'~expr :value expr})
(defmacro format-expr [expr] (format-obj expr))

(defmacro print-expr [expr] `(print ~(format-obj expr)))

(def str->int-xform
  (comp
   (map str/trim)
   (map #(Integer/parseInt %))))

(defn clean-input
  ([input-str] (clean-input input-str #"\n+"))
  ([input-str separator]
   (as-> input-str _input
     (str/split _input separator)
     (into [] str->int-xform _input)))) ; Have to wrap java methods in a function

(defn get-symbol-list
  "Parses a multi-line string into a listof symbols for parsing
  https://blog.michielborkent.nl/blog/2017/10/10/parsing-a-circuit-with-clojure-spec/

  Example
  (get-symbol-list \"[2344 test] \n [884] -> xis1\")"
  [input-string]
  (into [] (comp
            (map (partial format "[%s]"))
            (map edn/read-string))
        (str/split-lines input-string)))

#_(format-expr (first [1 2 3]))
#_(print-expr (first [1 2 3]))
