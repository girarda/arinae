(ns markov-clj.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn foobar [r t] (merge-with clojure.set/union r (let [[a b
c] t] {[a b] (if c #{c} #{})})))

(defn word-transitions [words]
  (partition-all 3 1 words))

(defn word-chain [word-transitions]
  (reduce foobar {} word-transitions))

(defn text-to-bigram [text]
  (defn words [s]
  (clojure.string/split s #"\s+"))
  (word-chain (word-transitions (words text))))