(ns markov-clj.utils)

(defn weighted-random-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #( <= % r) w)))))

(defn str->words [s]
  (clojure.string/split s #"\s+"))

(defn words->str [words]
  (apply str (interpose " " words)))

(defn word-transitions [words n]
  (partition-all n 1 words))

(defn str->triplets [s]
  (let [words (str->words s)]
    (word-transitions words 3)))