(ns fujiin.mjolniir.markov
  (:require [clj-http.client :as client]
            [clojure.string :as s]
            [fujiin.mjolniir.utils :refer [includes?]]))

;; For crawling purposes
;;
(def TITLES_URL "http://pastebin.com/raw.php?i=nqpsnTtW")

(defn crawl-titles []
  (-> (client/get TITLES_URL)
      :body
      (s/split #"[\n\r]+")))

;; ======= ;;

(defn tokenize [sent]
  (s/split (s/trim sent) #"[\ \s]+"))

(defn inc-in
  "Increments dual level map values, defaulting to 0"
  [m k1 k2]
  (update-in m [k1 k2] (fnil inc 0)))

(defn substring
  "Creates substring of tokenized sentence from <start> to <end>"
  [tokens start end]
  (let [t (count tokens)]
    (s/join " " (subvec tokens (min t start) (min t end)))))

(defn chains->map
  "Converts array of chains (formatted [phrase-1 phrase-2])
   into frequency map w/ format { phrase-1 { phrase-2 occurrences }}"
  [chains]
  (loop [current chains mapping {}]
    (let [c (first current)
          n (next current)
          m (apply inc-in (flatten [mapping c]))]
      (if (nil? n) m (recur n m)))))

(defn tokens->chains
  "Convert sentence into chains formatted [phrase-1 phrase-2].
   Max phrase lengths are defined by lookback"
  [tokens lookback]
  (for [i (-> tokens count inc range)]
     (let [term (substring tokens (max 0 (- i lookback)) i)
           follow (substring tokens i (inc i))]
       [term follow])))

(defn occurrence-map
  "Generates occurrence mapping for using titles and lookback"
  [titles lookback]
  (->> titles
       (map tokenize)
       (filter #(> (count %) lookback))
       (map #(tokens->chains % lookback))
       (reduce into)
       chains->map))

(defn distro->probs
  "Converts word distribution mapping into probability mapping.
  { word occurrences } => { word probability }"
  [distro]
  (let [sum (reduce + (map last distro))]
    (map (fn [[word occ]] {word (/ occ sum)})
         distro)))

(defn occ->probs
  "Remaps a single occurrence distribution into
   probability distribution"
  [[word distro]]
  {word (->> distro distro->probs (reduce into))})

(defn probability-map
  "Convert multiple occurrence mapping into probability mappings"
  [omap]
  (->> omap (map occ->probs) (reduce into)))

(defn process
  "Generates probability map given titles and lookback value"
  [titles lookback]
  (let [omap (occurrence-map titles lookback)
        prmap (probability-map omap)]
    {:titles titles
     :lookback lookback
     :data prmap }))

(defn sample
  "Generate next word based on probability distribution"
  [distro]
  (loop [pairs (seq distro)
         thresh 0
         prev nil]
    (let [[word v] (first pairs)
          n (next pairs)
          r (rand)
          t (+ thresh v)
          cand (if (< r (/ v t)) word prev)]
      (if-not n cand (recur n t cand)))))

(defn generate-sentence
  "Probabilistically generate sentence w/ given lookback and
   probabilities. 1st arg should be in output format of
   the #process function. Optional 2nd arg specifies word limit"
  [{:keys [lookback data] :as res} & [limit]]
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

(defn run
  "Wrapper for #generate-sentence with default settings.
   Will crawl for titles if not given"
  [& {:keys [titles lookback limit]
              :or {lookback 2}}]
  (let [titles (or titles (crawl-titles))]
    (loop []
      (let [res (process titles lookback)
            sent (generate-sentence res limit)
            out (->> sent (s/join " ") s/trim)]
        (if-not (includes? titles out) out (recur))))))
