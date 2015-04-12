(ns markov-clj.core)

(defn word-transitions [words]
  (partition-all 3 1 words))

(defn word-chain [word-transitions]
  (reduce (fn [r t]
            (merge-with clojure.set/union r
                        (let [[a b c] t]
                          {[a b] (if c #{c} #{})})))
           {} word-transitions))

(defn str->words [s]
  (clojure.string/split s #"\s+"))

(defn weighted-random-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #( <= % r) w)))))

(defn lexicon [text]
  (set (str->words text)))

;;;
;;;
;;; Please Alex, clean these 5 functions :(
;;;
;;;

(defn count-transitions [word-chains]
  (reduce #(assoc %1 %2
             (inc (%1 %2 0)))
          {} word-chains))

(defn count-suffix-frequency [transitions]
  (map (fn [transition]
         (let [r (reverse transition)
               [c [a b suffix]] r
               bigram [a b]]
              [bigram (if (nil? suffix) [] [{suffix c}])])) transitions))

(defn merge-bigram-suffixes [bigrams]
  (reduce #(assoc %1 (first %2)
             (concat (last %2) (get %1 (first %2))))
          {} bigrams))

(defn foo [bigrams]
  (into {} (map (fn [m] (let [[prefix suffixes] m] {prefix (into {} suffixes)})) bigrams)))

(defn text->chain [text]
  (foo (merge-bigram-suffixes (count-suffix-frequency (count-transitions (word-transitions (str->words text)))))))

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
      (let [suffix (weighted-random-choice suffixes)
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

(defn generate-text [start-phrase word-chain]
  (let [prefix (str->words start-phrase)
        result-chain (walk-chain prefix word-chain)
        result-text (chain->text result-chain)]
    result-text))

(defn process-file [fname]
  (text->chain
    (slurp (clojure.string/join ["resources/" fname]))))

(def files ["quangle-wangle.txt" "pelican.txt" "pobble.txt"])

(def functional-leary (apply merge-with clojure.set/union (map process-file files)))

; Prefix list to sound like Edward
(def prefix-list ["On the" "They went" "And all" "We think"
                  "For every" "No other" "To a" "And every"
                  "We, too," "For his" "And the" "But the"
                  "Are the" "The Pobble" "For the" "When we"
                  "In the" "Yet we" "With only" "Are the"
                  "Though the"  "And when"
                  "We sit" "And this" "No other" "With a"
                  "And at" "What a" "Of the"
                  "O please" "So that" "And all" "When they"
                  "But before" "Whoso had" "And nobody" "And it's"
                  "For any" "For example," "Also in" "In contrast"])

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (clojure.string/replace result-text #"[,| ]$" ".")]
    (clojure.string/replace cleaned-text "\"" "'")))

(defn tweet-text []
  (let [text (generate-text (first (shuffle prefix-list)) functional-leary)]
    (end-at-last-punctuation text)))
