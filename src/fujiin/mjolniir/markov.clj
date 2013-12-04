(ns fujiin.mjolniir.markov
  (:require [clj-http.client :as client]
            [clojure.string :as s]))

(def TITLES_URL "http://pastebin.com/raw.php?i=nqpsnTtW")

(defn crawl-titles []
  (-> (client/get TITLES_URL)
      :body
      (s/split #"[\n\r]+")))

(defn update-map [m k1 k2]
  (update-in m [k1 k2] (fnil inc 0)))

(defn chain-map [sent lookback]
  (for [i (-> sent count dec range)]))

(defn occ-map [ws lookback mmap]
  (loop [i 0
         m mmap
         tokens (vec ws)]
    (let [j (inc i)
          t (count tokens)
          w1 (s/join " " (subvec ws (max 0 (- i lookback)) i))
          w2 (s/join " " (subvec ws i j))
          v (m w1)
          newm (update-in m [w1 w2] (fnil inc 0))]
      (if (>= j t)
        newm
        (recur j newm tokens)))))

(defn gen-occ-map [titles lookback]
  (loop [ts titles
         m {}]
    (let [ws (s/split (s/trim (first ts)) #"[\ \s]+")
          newm (if (> (count ws) lookback)
                 (occ-map ws lookback m)
                 m)
          jobs (rest ts)]
      (if (empty? jobs)
        newm
        (recur jobs newm)))))

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
    (s/join " " (gen-sent res limit))))
