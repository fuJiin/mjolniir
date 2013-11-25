(ns mjolniir.core
  (:require [marcliberatore.mallet-lda :refer [lda make-instance-list]]
            [clojure.string :as s]
            [clojure.pprint :refer :all]))

(defn default-tokenizer [sent]
  (s/split sent #"[\ \s]+"))

(defn tokenize
  ([sent tokenizer] (tokenizer sent))
  ([sent] (default-tokenizer sent)))

(defn create-tokens [sent]
  (-> sent
      (s/replace #"[\s\W]+" " ")
      s/trim
      s/lower-case
      tokenize))

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
