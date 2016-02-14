(ns parser-combinators.parsers
  (:require [parser-combinators.input :as input]))

(defn p-any
  "Matches any individual character"
  []
  (fn [input]
    (let [v (input/input-read input)]
      {:input (input/input-advance input 1)
       :result v})))

(defn p-oneof
  "Matches any single given character in the string s"
  [s]
  (fn [input]
    (let [v (input/input-read input)]
      (if (some #{v} s)
        {:input (input/input-advance input 1)
         :result v}
        {:input input
         :result :failure}))))
