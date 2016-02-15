(ns parser-combinators.math
  (:require [parser-combinators.parsers :as p]))

(defn math-fold
  [[x op y]]
  {:pre [x op y]}
  (let [x (Integer/parseInt (clojure.string/join x))
        y (Integer/parseInt (clojure.string/join y))]
    (case op
      \* (* x y)
      \/ (/ x y)
      \+ (+ x y)
      \- (- x y)
      :failure)))

(declare p-expr)
(declare p-factor)
(declare p-term)
(declare p-math)

(defn p-expr
  []
  (fn [input]
    ((p/p-or (p/p-folding-and math-fold
                              (p-factor)
                              (p/p-oneof "+-")
                              (p-factor))
             (p-factor))
     input)))

(defn p-factor
  []
  (fn [input]
    ((p/p-or (p/p-folding-and math-fold
                            (p-term)
                            (p/p-oneof "*/")
                            (p-term))
             (p-term))
     input)))

(defn p-term
  []
  (fn [input]
    ((p/p-or (p/p-int)
             (p/p-parens (p-expr)))
     input)))

(defn p-math
  []
  (fn [input]
    ((p/p-whole (p-expr))
     input)))
