(ns markov-clj.core-test
  (:require [clojure.test :refer :all]
            [markov-clj.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

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
             (text-to-bigram example))))))