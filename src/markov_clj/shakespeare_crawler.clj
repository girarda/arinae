(ns markov-clj.shakespeare-crawler
  (:require [net.cgrand.enlive-html :as html]))

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn get-sonnet [number]
  (defn extract-content [m]
  (first (get m :content)))
  (clojure.string/lower-case (clojure.string/join "\n" (filter identity (map extract-content (get (nth (get (nth (get (nth (get (second (get (nth (get (first (fetch-url (str "http://shakespeares-sonnets.com/sonnet/" number))) :content) 3) :content)) :content) 5) :content) 1) :content) 3) :content))))))

(defn get-sonnets-from-range [a b]
  (map (fn [i]
         (try (get-sonnet i)
           (catch Exception e (str "caught exception for sonnet" i ": " (.getMessage e)))))
       (range a b)))