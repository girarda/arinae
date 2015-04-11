(ns markov-clj.core)

(defn word-transitions [words]
  (partition-all 3 1 words))

(defn word-chain [word-transitions]
  (reduce (fn [r t]
            (merge-with clojure.set/union r
                        (let [[a b c] t]
                          {[a b] (if c #{c} #{})})))
           {} word-transitions))

(defn text-to-bigram [text]
  (defn words [s]
    (clojure.string/split s #"\s+"))
  (word-chain (word-transitions (words text))))

(defn chain->text [chain]
  (apply str (interpose " " chain)))

; Takes the prefix and get the suffixes associated with it
; If there are no suffixes, terminate and return the result
; Otherwise, shuffle to pick a random suffix
; The, construct the new prefix from the last part of the current prefix and suffix
; Recurs into the function using the new-prefix and adding the suffix to the result
(defn walk-chain-recurs [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]
            result-with-spaces (chain->text result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn walk-chain [prefix chain]
  (walk-chain-recurs prefix chain prefix))

