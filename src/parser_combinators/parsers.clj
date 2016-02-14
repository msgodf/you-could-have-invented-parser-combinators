(ns parser-combinators.parsers
  (:require [parser-combinators.input :as input]))

(defn lit
  [c]
  (fn parser [input]
    (if (= c (input/input-read input))
      {:input (input/input-advance input 1)
       :result c}
      {:input input
       :result :failure})))

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

(defn p-and
  "This takes a variable number of parsers, and succeeds if all of the parsers
   succeed, specifically each successive parser succeeds on each successive
   input character."
  [& parsers]
  (fn [input]
    {:pre [(:sequence input) (:position input)]}
    (reduce (fn [input parser]
              (let [{result :result input0 :input} (parser (:input input))]
                (if (= :failure result)
                  (reduced {:input input0
                            :result :failure})
                  {:input input0
                   :result (conj (or (:result input) [])
                                 result)})))
            {:input input
             :result []}
            parsers)))

