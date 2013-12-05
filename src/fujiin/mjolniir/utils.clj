(ns fujiin.mjolniir.utils)

(defn includes?
  "Checks if coll has a value equal to elm"
  [coll elm]
  (or (some #(= elm %) coll)
      false))
