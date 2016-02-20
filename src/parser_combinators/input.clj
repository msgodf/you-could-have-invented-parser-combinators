(ns parser-combinators.input)

(defn input
  "Construct a new input from a sequence, with the position set to zero"
  [value]
  {:sequence value
   :position 0})

(defn input-read
  [input]
  {:pre [(:position input) (:sequence input)]}
  (let [{position :position xs :sequence} input]
    (when (< position (count xs))
      (nth xs position))))

(defn input-advance
  [input n]
  (update-in input
             [:position]
             (fn [v] (+ v n))))
