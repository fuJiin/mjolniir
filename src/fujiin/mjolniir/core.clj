(ns fujiin.mjolniir.core
  (:require [marcliberatore.mallet-lda :refer [lda make-instance-list]]
            [clojure.string :as s]
            [fujiin.mjolniir.stopwords :refer [get-stopwords]]
            [fujiin.mjolniir.utils :refer [includes?]])
  (:import (cc.mallet.pipe TokenSequenceRemoveStopwords)))

(defn scrub-stopwords [tokens]
  (let [stopwords (vec (get-stopwords))]
    (remove (fn [word]
              (some #(= word %) stopwords))
            tokens)))

(defn create-tokens [sent]
  (let [tokens (-> sent
                   s/lower-case
                   (s/replace #"[\s\W]+" " ")
                   (s/split #"[\ \s]+")
                   scrub-stopwords)]
    (scrub-stopwords (map s/trim tokens))))

(defn create-instance-data [docs]
  (map-indexed (fn [i s] [i (create-tokens s)]) docs))

(defn create-instance-list [docs]
  (let [data (create-instance-data docs)]
    (make-instance-list data)))

(defn speller [lookup]
  (fn [word]
    (.lookupObject lookup (.getID word))))

(defn word-fn [lookup]
  (fn [word]
    (let [spell (speller lookup)]
      {:word (spell word)
       :weight (.getWeight word)})))

(defn instance-fn [model lookup]
  (fn [i instance]
    (let [word-groups (.getSortedWords model)
          probs (.getTopicProbabilities model i)
          spell (speller lookup)]
      (map-indexed (fn [j words]
                     {:topics (map (word-fn lookup) words)
                      :probability (nth probs j)})
           word-groups))))

(defn topic-model [docs]
  (let [instances (create-instance-list docs)
        lookup (.getDataAlphabet instances)
        model (lda instances)]
    (map-indexed (fn [i inst]
                   (let [f (instance-fn model lookup)]
                     {:doc (nth docs i)
                      :data (f i inst)}))
                 instances)))
