(ns fujiin.mjolniir.stopwords
  (:require [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as select]
            [net.cgrand.enlive-html :as html]
            [net.cgrand.tagsoup :as tagsoup]
            [net.cgrand.xml :as xml]))

(def STOPWORDS_URL "http://www.ranks.nl/resources/stopwords.html")

(defn fetch-url
  ([uri] (client/get uri))
  ([] (client/get STOPWORDS_URL)))

(defn extract-title [content]
  (html/select content [#{:legend :h4}]))

(defn has-stop-words? [content]
  (seq (extract-title content)))

(defn extract-stopwords [blocks]
  (let [words (flatten (map :content blocks))]
    (filter string? words)))

(defn process [html]
  (let [doc (html/html-snippet html)
        contents (html/select doc [:div.content])
        blocks (filter has-stop-words? contents)]
    (extract-stopwords (flatten
                        (map #(html/select % [:td]) blocks)))))

(defn write [data]
  (spit "data/stopwords.txt" data))

(defn run []
  (let [words (process (:body (fetch-url)))]
    (write (vec words))))
