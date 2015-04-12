(ns markov-clj.core
  (:require [net.cgrand.enlive-html :as html]
            [overtone.at-at :as overtone]
            [twitter.api.restful :as twitter]
            [twitter.oauth :as  twitter-oauth]
            [environ.core :refer [env]]))

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

(defn extract-content [m]
  (first (get m :content)))

(defn get-all-keys [map1 map2]
  (distinct (concat (keys map1) (keys map2))))

(defn merge-bigram [bigram chain1 chain2] [bigram (apply merge-with + [(get chain1 bigram) (get chain2 bigram)])])

(defn merge-bigrams-into-vector [chain1 chain2] (map (fn [k] (merge-bigram k chain1 chain2)) (get-all-keys chain1 chain2)))

(defn merge-bigrams [chain1 chain2] (foo (merge-bigrams-into-vector chain1 chain2)))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn get-sonnet [number]
  (clojure.string/lower-case (clojure.string/join "\n" (filter identity (map extract-content (get (nth (get (nth (get (nth (get (second (get (nth (get (first (fetch-url (str "http://shakespeares-sonnets.com/sonnet/" number))) :content) 3) :content)) :content) 5) :content) 1) :content) 3) :content))))))

(defn get-bigrams-from-sonnets-range [a b]
  (reduce merge-bigrams (map text->chain (map (fn [x] (get-sonnet x)) (range a b)))))

(def functional-leary (apply merge-with clojure.set/union (map process-file files)))

; Prefix list to sound like Shakespeare
(def prefix-list ["from fairest" "when forty" "look in" "upon thy"
                  "the lovely" "then were" "to a" "when I"
                  "then of," "when in" "and look" "to the"
                  "but I" "the Pobble" "for the" "when we"
                  "in the" "my love" "how can" "when love"
                  "i have"  "when you"
                  "if my" "but all" "in the" "with a"
                  "not mine" "what a" "if thy"
                  "o please" "those lips" "i do" "when my"
                  "how can" "that thou" "and yet" "so now"
                  "when in" "your love," "take all" "that you"])

(defn end-at-last-punctuation [text]
  (let [trimmed-to-last-punct (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        trimmed-to-last-word (apply str (re-seq #".*[^a-zA-Z]+" text))
        result-text (if (empty? trimmed-to-last-punct)
                      trimmed-to-last-word
                      trimmed-to-last-punct)
        cleaned-text (clojure.string/replace result-text #"[,| ]$" ".")]
    (clojure.string/replace cleaned-text "\"" "'")))

(defn tweet-text [bigrams]
  (let [text (generate-text (first (shuffle prefix-list)) bigrams)]
    (end-at-last-punctuation text)))

(def my-creds (twitter-oauth/make-oauth-creds (env :app-consumer-key)
                                               (env :app-consumer-secret)
                                               (env :user-access-token)
                                               (env :user-access-secret)))

(defn status-update [bigrams]
  (let [tweet (tweet-text bigrams)]
    (println "generated tweet is: " tweet)
    (println "char count is: " (count tweet))
    (when (not-empty tweet)
      (try (twitter/statuses-update :oauth-creds my-creds
                                    :params {:status tweet})
        (catch Exception e (println "Oh no! " (.getMessage e)))))))

(def my-pool (overtone/mk-pool))

(defn -main [& args]
  ;; every 8 hours
  (println "Started up")
  (def bigrams (get-bigrams-from-sonnets-range 1 51))
  (println (tweet-text bigrams))
  (overtone/every (* 1000 60 60 8) #(println (status-update bigrams)) my-pool))
