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

(defn p-folding-and
  "This takes a folding function and a variable number of parsers, and if
   all of the parsers succeed in sequence on the input, then it calls the
   'folding' function on the sequence of results."
  [f & parsers]
  {:pre [(fn? f)]}
  (fn [input]
    {:pre [(:sequence input) (:position input)]}
    (let [{result :result input :input} ((apply p-and parsers) input)]
      (if (= :failure result)
        {:input input
         :result :failure}
        {:input input
         :result (f result)}))))

(defn p-many
  "This takes a parser, and runs it zero or more times on the input, until it fails."
  [parser]
  (fn [input]
    {:pre [(:sequence input) (:position input)]}
    (reduce (fn [input parser]
              (let [{result :result input0 :input} (parser (:input input))]
                (if (= :failure result)
                  (reduced {:input input0
                            :result (:result input)})
                  {:input input0
                   :result (conj (or (:result input) [])
                                 result)})))
            {:input input
             :result []}
            (repeat parser))))
(defn p-soi
  "Parser that succeeds on the start of the input, and fails otherwise"
  []
  (fn [input]
    (if (= 0 (:position input))
      {:input input
       :result nil}
      {:input input
       :result :failure})))

