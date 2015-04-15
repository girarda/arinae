(ns markov-clj.core
  (:require [net.cgrand.enlive-html :as html]
            [overtone.at-at :as overtone]
            [markov-clj.shakespeare-crawler :as shake]
            [markov-clj.twitter-facade :as twitface]
            [markov-clj.utils :as utils]))

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

(defn text->bigrams [text]
  (let [word-triplets (utils/str->triplets text)
        transitions-count (count-transitions word-triplets)]
  (foo (merge-bigram-suffixes (count-suffix-frequency transitions-count)))))

; Takes the prefix and get the suffixes associated with it
; If there are no suffixes, terminate and return the result
; Otherwise, shuffle to pick a random suffix
; The, construct the new prefix from the last part of the current prefix and suffix
; Recurs into the function using the new-prefix and adding the suffix to the result
(defn walk-chain-recurs [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (utils/weighted-random-choice suffixes)
            new-prefix [(last prefix) suffix]
            result-with-spaces (utils/words->str result)
            result-char-count (count result-with-spaces)
            suffix-char-count (inc (count suffix))
            new-result-char-count (+ result-char-count suffix-char-count)]
        (if (>= new-result-char-count 140)
          result
          (recur new-prefix chain (conj result suffix)))))))

(defn walk-chain [prefix chain]
  (walk-chain-recurs prefix chain prefix))

(defn generate-text [start-phrase word-chain]
  (let [prefix (utils/str->words start-phrase)
        result-chain (walk-chain prefix word-chain)
        result-text (utils/words->str result-chain)]
    result-text))

(defn get-all-keys [map1 map2]
  (distinct (concat (keys map1) (keys map2))))

(defn merge-bigram [bigram chain1 chain2]
  [bigram (apply merge-with
                 +
                 [(get chain1 bigram) (get chain2 bigram)])])

(defn merge-bigrams-into-vector [bigram1 bigram2]
  (let [all-prefixes (get-all-keys bigram1 bigram2)]
    (map (fn [k]
         (merge-bigram k bigram1 bigram2))
       all-prefixes)))

(defn merge-bigrams [chain1 chain2]
  (foo (merge-bigrams-into-vector chain1 chain2)))

(defn get-bigrams-from-corpus [corpus]
  (reduce merge-bigrams (map text->bigrams corpus)))

(defn get-bigrams-from-sonnets-range [a b]
  (get-bigrams-from-corpus (shake/get-sonnets-from-range a b)))

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (clojure.string/replace result-text #"[,| ]$" ".")]
    (clojure.string/replace cleaned-text "\"" "'")))

(defn generate-tweet [bigrams]
  (defn choose-starting-bigram [bigrams]
    (clojure.string/join " " (first (shuffle (keys bigrams)))))
  (let [text (generate-text (choose-starting-bigram bigrams) bigrams)]
    (end-at-last-punctuation text)))

(defn send-tweet [bigrams]
  (let [tweet (generate-tweet bigrams)]
    (println "generated tweet is: " tweet)
    (println "char count is: " (count tweet))
    (twitface/status-update tweet)))

(def my-pool (overtone/mk-pool))

(defn -main [& args]
  ;; every 8 hours
  (println "Started up")
  (def bigrams (get-bigrams-from-sonnets-range 1 155))
  (println (generate-tweet bigrams))
  (overtone/every (* 1000 60 60 8) #(println (send-tweet bigrams)) my-pool))
