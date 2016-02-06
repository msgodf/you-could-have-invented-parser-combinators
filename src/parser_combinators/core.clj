(ns parser-combinators.core)

;; Based on the post about parser combinators, with examples in Javascript
;; http://theorangeduck.com/page/you-could-have-invented-parser-combinators

;; Oh wait, this code is mutating - it changes input conditionally
;; `input` is a reference, not a value
;; I can do the same by using an atom

(defn input-read
  [input]
  (first @input))

(defn input-advance
  [input n]
  (swap! input
         (partial drop n)))

(defn lit
  [c]
  (fn parser [input]
    (if (= c (input-read input))
      (do (input-advance input 1)
          c)
      :failure)))

#_(let [input (atom (seq "abcd"))
        parser (lit \a)]
    (parser input))

(defn p-or
  [parser0 parser1]
  (fn parser [input]
    (let [result0 (parser0 input)]
      (if (not= :failure result0)
        result0
        (let [result1 (parser1 input)]
          (if (not= :failure result1)
            result1
            :failure))))))

#_(let [input (atom (seq "abcd"))
        parser (p-or (lit \a) (lit \b))]
    (parser input))
