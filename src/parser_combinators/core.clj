(ns parser-combinators.core)

;; Based on the post about parser combinators, with examples in Javascript
;; http://theorangeduck.com/page/you-could-have-invented-parser-combinators

;; Oh wait, this code is mutating - it changes input conditionally
;; `input` is a reference, not a value
;; I can do the same by using an atom

(defn input-read
  [input]
  (let [{position :position xs :sequence} input]
    (nth xs position)))

(defn input-advance
  [input n]
  (update-in input
             [:position]
             (fn [v] (+ v n))))

(defn input-get
  [input]
  (:position input))

(defn input-set
  [input n]
  (update-in input
             [:position]
             (fn [_] n)))

(defn lit
  [c]
  (fn parser [input]
    (if (= c (input-read input))
      {:input (input-advance input 1)
       :result c}
      {:input input
       :result :failure})))

(defn p-or
  [parser0 parser1]
  (fn parser [input]
    (let [{result0 :result input :input} (parser0 input)]
      (if (not= :failure result0)
        {:input input :result result0}
        (let [{result1 :result input :input} (parser1 input)]
          (if (not= :failure result1)
            {:input input :result result1}
            {:input input :result :failure}))))))

;; Now to implement `and`

;; This gets and sets the position in the input
;; i.e. manipulates it, because it needs to advance and rewind

;; this means that the input isn't (necessarily) mutable, but the position into
;; it can change

;; So instead of just a sequence, make `input` a map of a sequence and a position
;; `input-read` gets the value at the current position.
;; `input-advance` increments the current position by a given offset.
;; `input-get` gets the current position, and `input-set` sets the current
;; position.)

;; {:sequence xs :position n}

;; although that doesn't suit using an atom very well

(defn p-and
  [parser0 parser1]
  (fn parser [input]
    (let [pos (input-get input)
          {result0 :result input :input} (parser0 input)]
      (if (= :failure result0)
        {:input (input-set input pos)
         :result :failure}
        (let [{result1 :result input :input} (parser1 input)]
          (if (= :failure result1)
            {:input (input-set input pos)
             :result :failure}
            {:input input
             :result (str result0 result1)}))))))

;; At this point there was no real point in using the atom, because the input
;; position could have been changed between getting it and setting it.

;; I'd prefer not to use mutation at all - but I should use a ref instead of an
;; atom

;; How to avoid mutation?

;; Would have to take input and return the modified one - lit has backtracking, the parser it generates can return both a new input and a result

;; parsers take an input map like {:position n :sequence xs} and return a map that's something like {{:input {:position n :sequence xs} :result x}
;; perhaps it's a good idea if they also take the result? Let's try the first way for now.
