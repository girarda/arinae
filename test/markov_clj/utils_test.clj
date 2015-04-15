(ns markov-clj.utils_test
  (:require [clojure.test :refer :all]
            [markov-clj.utils :refer :all]))

(deftest test-str->words
  (testing "it splits a string into a sequence of words"
    (let [example "this is an example"
          words '("this" "is" "an" "example")]
      (is (= (str->words example)
             words)))))

(deftest test-words->str
  (testing "it merges a seq of words into a string"
    (let [words '("this" "is" "an" "example")
          sentence "this is an example"]
      (is (= (words->str words)
             sentence)))))

(deftest test-word-transitions
  (testing "it splits a sequence of words in triplets"
    (let [example ["this" "is" "an" "example"]
          triplets '(("this" "is" "an")
                    ("is" "an" "example")
                    ("an" "example")
                    ("example"))]
      (is (= (word-transitions example 3)
             triplets)))))

(deftest test-str->triplets
  (testing "it splits a sequence of words in triplets"
    (let [example "this is an example"
          triplets '(("this" "is" "an")
                    ("is" "an" "example")
                    ("an" "example")
                    ("example"))]
      (is (= (str->triplets example)
             triplets)))))