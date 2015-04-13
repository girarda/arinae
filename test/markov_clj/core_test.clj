(ns markov-clj.core-test
  (:require [clojure.test :refer :all]
            [markov-clj.core :refer :all]))

(deftest test-word-transitions
  (testing "it splits a sequence of words in triplets"
    (let [example ["this" "is" "an" "example"]
          triplets '(("this" "is" "an")
                    ("is" "an" "example")
                    ("an" "example")
                    ("example"))]
      (is (= (word-transitions example)
             triplets)))))

(deftest test-str->words
  (testing "it splits a string into a sequence of words"
    (let [example "this is an example"
          words '("this" "is" "an" "example")]
      (is (= (str->words example)
             words)))))

(deftest test-word-chain
  (testing "it produces a chain of possible bigrams"
    (let [example '(("And" "the" "Golden")
                    ("the" "Golden" "Grouse")
                    ("And" "the" "Pobble")
                    ("the" "Pobble" "who"))]
      (is (= {["the" "Pobble"] #{"who"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (word-chain example))))))

(deftest test-text-to-word-chain
  (testing "it produces a chain of possible bigrams from text"
    (let [example "And the Golden Grouse\nAnd the Pobble who"]
     (is (= {["who" nil] {}
             ["Pobble" "who"] {}
             ["the" "Pobble"] {"who" 1}
             ["Grouse" "And"] {"the" 1}
             ["Golden" "Grouse"] {"And" 1}
             ["the" "Golden"] {"Grouse" 1}
             ["And" "the"] {"Pobble" 1 "Golden" 1}}
             (text->chain example))))))

(deftest test-get-bigrams-from-multiple-strings
  (testing "it produces a bigram from the multiple strings"
    (let [first-string "hello yes this is dog"
          second-string "hello yes this isnt cat"
          third-string "hello no this isnt frog"
          corpus [first-string second-string third-string]
          merged-bigram {["hello" "yes"] {"this" 2}
                         ["yes" "this"] {"is" 1 "isnt" 1}
                         ["this" "is"] {"dog" 1}
                         ["this" "isnt"] {"cat" 1 "frog" 1}
                         ["is" "dog"] {}
                         ["isnt" "cat"] {}
                         ["dog" nil] {}
                         ["cat" nil] {}
                         ["hello" "no"] {"this" 1}
                         ["no" "this"] {"isnt" 1}
                         ["isnt" "frog"] {}
                         ["frog" nil] {}}]
    (is (= merged-bigram (get-bigrams-from-corpus corpus))))))

(deftest test-walk-chain
  (let [chain {["who" nil] {}
             ["Pobble" "who"] {}
             ["the" "Pobble"] {"who" 1}
             ["Grouse" "And"] {"the" 1}
             ["Golden" "Grouse"] {"And" 1}
             ["the" "Golden"] {"Grouse" 1}
             ["And" "the"] {"Pobble" 1 "Golden" 1}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain prefix chain))))
      (testing "multiple choices"
        (with-redefs [weighted-random-choice (fn [c] (first (keys c)))]
          (let [prefix ["And" "the"]]
           (is (= ["And" "the" "Pobble" "who"]
                  (walk-chain prefix chain)))))
        (testing "repeating chains"
          (with-redefs [weighted-random-choice (fn [c] (last (keys c)))]
                       (let [prefix ["And" "the"]]
                         (is (> 140
                                (count (apply str (walk-chain prefix chain)))))
                         (is (= ["And" "the" "Golden" "Grouse" "And" "the" "Golden" "Grouse"]
                                (take 8 (walk-chain prefix chain)))))))))))

(deftest test-generate-text
  (with-redefs [weighted-random-choice (fn [c] (first (keys c)))]
    (let [chain {["who" nil] {}
             ["Pobble" "who"] {}
             ["the" "Pobble"] {"who" 1}
             ["Grouse" "And"] {"the" 1}
             ["Golden" "Grouse"] {"And" 1}
             ["the" "Golden"] {"Grouse" 1}
             ["And" "the"] {"Pobble" 1 "Golden" 1}}]
      (is (= "the Pobble who" (generate-text "the Pobble" chain)))
      (is (= "And the Pobble who" (generate-text "And the" chain))))))

(deftest test-end-at-last-punctuation
  (testing "Ends at the last punctuation"
    (is (= "In a tree so happy are we."
           (end-at-last-punctuation "In a tree so happy are we. So that")))
    (testing "Replaces ending comma with a period"
      (is (= "In a tree so happy are we."
             (end-at-last-punctuation "In a tree so happy are we, so that"))))
      (testing "If there are no previous punctuations, remove last word and add one at the end"
        (is (= "In the light of the blue moon."
               (end-at-last-punctuation "In the light of the blue moon there"))))
      (testing "works with multiple punctuations"
        (is (= "In the light of the blue moon. We danced merrily."
               (end-at-last-punctuation "In the light of the blue moon. We danced merrily. Be"))))))