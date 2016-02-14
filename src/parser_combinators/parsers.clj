(ns parser-combinators.parsers
  (:require [parser-combinators.input :as input]))

(defn p-any
  []
  (fn [input]
    (let [v (input/input-read input)]
      {:input (input/input-advance input 1)
       :result v})))
