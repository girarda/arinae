(ns markov-clj.core-test
  (:require [clojure.test :refer :all]
            [markov-clj.core :refer :all]))

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
     (is (= {["who" nil] #{}
             ["Pobble" "who"] #{}
             ["the" "Pobble"] #{"who"}
             ["Grouse" "And"] #{"the"}
             ["Golden" "Grouse"] #{"And"}
             ["the" "Golden"] #{"Grouse"}
             ["And" "the"] #{"Pobble" "Golden"}}
             (text->chain example))))))

(deftest test-walk-chain
  (let [chain {["who" nil] #{}
             ["Pobble" "who"] #{}
             ["the" "Pobble"] #{"who"}
             ["Grouse" "And"] #{"the"}
             ["Golden" "Grouse"] #{"And"}
             ["the" "Golden"] #{"Grouse"}
             ["And" "the"] #{"Pobble" "Golden"}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain prefix chain))))
      (testing "multiple choices"
        (with-redefs [shuffle (fn [c] c)]
          (let [prefix ["And" "the"]]
           (is (= ["And" "the" "Pobble" "who"]
                  (walk-chain prefix chain)))))
        (testing "repeating chains"
          (with-redefs [shuffle (fn [c] (reverse c))]
                       (let [prefix ["And" "the"]]
                         (is (> 140
                                (count (apply str (walk-chain prefix chain)))))
                         (is (= ["And" "the" "Golden" "Grouse" "And" "the" "Golden" "Grouse"]
                                (take 8 (walk-chain prefix chain)))))))))))

(deftest test-generate-text
  (with-redefs [shuffle (fn [c] c)]
    (let [chain {["who" nil] #{}
               ["Pobble" "who"] #{}
               ["the" "Pobble"] #{"who"}
               ["Grouse" "And"] #{"the"}
               ["Golden" "Grouse"] #{"And"}
               ["the" "Golden"] #{"Grouse"}
               ["And" "the"] #{"Pobble" "Golden"}}]
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