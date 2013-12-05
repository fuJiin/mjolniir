(ns fujiin.mjolniir.markov
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [fujiin.mjolniir.utils :refer [includes?]]))

(def TITLES_URL "http://pastebin.com/raw.php?i=nqpsnTtW")

(defn crawl-titles []
  (-> (client/get TITLES_URL)
      :body
      (s/split #"[\n\r]+")))

(defn tokenize [sent]
  (s/split (s/trim sent) #"[\ \s]+"))

(defn update-map [m k1 k2]
  (update-in m [k1 k2] (fnil inc 0)))

(defn words [sent start end]
  (let [t (count sent)]
    (s/join " " (subvec sent (min t start) (min t end)))))

(defn join-chains [chains]
  (loop [current chains mapping {}]
    (let [c (first current)
          n (next current)
          m (apply update-map (flatten [mapping c]))]
      (if (nil? n) m (recur n m)))))

(defn map-chains [tokens lookback]
  (for [i (-> tokens count inc range)]
     (let [term (words tokens (max 0 (- i lookback)) i)
           follow (words tokens i (inc i))]
       [term follow])))

(defn occurrence-map [titles lookback]
  (->> titles
       (map tokenize)
       (filter #(> (count %) lookback))
       (map #(map-chains % lookback))
       (reduce into)
       join-chains))

(defn distro->probs
  "Converts word distribution mapping into probability mapping.
  { word occurrences } -> { word probability }"
  [distro]
  (let [sum (reduce + (map last distro))]
    (map (fn [[word occ]] [word (/ occ sum)])
         distro)))

(defn occ->probs [[word distro]]
  {word (->> (distro->probs distro)
             flatten
             (apply hash-map))})

(defn probability-map [omap]
  (->> omap
       (map occ->probs)
       (reduce into)))

(defn process [titles lookback]
  (let [omap (occurrence-map titles lookback)
        prmap (probability-map omap)]
    {:titles titles
     :lookback lookback
     :data prmap }))

(defn sample [its]
  (loop [i 0
         acc 0
         items (vec its)
         w nil]
    (let [r (rand)
          [word v] (get items i)
          t (+ acc v)
          nextw (if (< r (/ v t))
                  word
                  w)
          j (inc i)]
      (if (>= j (count items))
        nextw
        (recur j t items nextw)))))

(defn gen-sent [{:keys [lookback data] :as res} & [limit]]
  (loop [tokens []]
    (let [prev (take-last lookback tokens)
          k (if (seq prev) (s/join " " prev) "")
          its (get data k)
          nextw (if (seq its) (sample its) nil)
          sent (if nextw (conj tokens nextw) tokens)]
      (if (or (nil? (seq nextw))
              (and limit (>= (count sent) (or limit 0))))
        sent
        (recur sent)))))

(defn run [& {:keys [titles lookback limit]
              :or {lookback 2}}]
  (let [titles (or titles (crawl-titles))]
    (loop []
      (let [res (process titles lookback)
            out (->> (gen-sent res limit)
                     (s/join " ")
                     (s/trim))]
        (if-not (includes? titles out)
          out
          (recur))))))
