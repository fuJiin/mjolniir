(ns fujiin.mjolniir.utils)

(defn includes? [coll elm]
  "Checks if coll has a value equal to elm"
  (or (some #(= elm %) coll)
      false))
