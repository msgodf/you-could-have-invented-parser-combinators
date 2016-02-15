(ns parser-combinators.input)

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
