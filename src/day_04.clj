(ns day-04
  (:require [clojure.string :as str]))

(defn- all-letters?
  [phrase-list]
  (every? #(re-matches #"[a-zA-Z]+" %) phrase-list))

(defn- has-anagrams?
  [phrase-list]
  (->> (map sort phrase-list)
       (group-by identity)
       vals
       (map count)
       (some #(> % 1))))

(defn valid-passphrase?
  [phrase]
  (let [phrase-list (str/split phrase #"\s")]
    (and (all-letters? phrase-list)
         (not (has-anagrams? phrase-list)))))

(defn valid-passphrases [passphrases-str]
  (as-> passphrases-str _in
    (str/split _in #"\n")
    (map str/trim _in)
    (filter valid-passphrase? _in)
    #_(count _in)))

#_(count (valid-passphrases input))

#_(valid-passphrase? "a b c d")
#_(valid-passphrase? "a1 b c d")
#_(valid-passphrase? "a b c d a")
#_(valid-passphrase? "a b c d a")
