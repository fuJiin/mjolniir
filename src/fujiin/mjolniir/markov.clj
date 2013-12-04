(ns fujiin.mjolniir.markov
  (:require [clj-http.client :as client]
            [clojure.string :as s]))

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

(defn gen-occ-map [titles lookback]
  (->> titles
       (map tokenize)
       (filter #(> (count %) lookback))
       (map #(map-chains % lookback))
       (reduce into)
       join-chains))

(defn gen-prob-map [omap]
  (->> omap
       (pmap
        (fn [[word distro]]
          (let [sum (reduce + (map last distro))]
            {word (->> distro
                           (map (fn [[k v]]
                                  [k (/ v sum)]))
                           flatten
                           (apply hash-map))})))
       (reduce into)))

(defn process [titles lookback]
  (let [omap (gen-occ-map titles lookback)
        prmap (gen-prob-map omap)]
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
  (let [titles (or titles (crawl-titles))
        res (process titles lookback)]
    (->> (gen-sent res limit)
         (s/join " ")
         (s/trim))))
