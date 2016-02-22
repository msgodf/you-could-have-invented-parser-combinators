(ns parser-combinators.parsers
  (:require [parser-combinators.input :as input]))

(defn lit
  [c]
  (fn parser [input]
    (if-let [v (input/input-read input)]
      (if (= c v)
        {:input (input/input-advance input 1)
         :result c}
        {:input input
         :result :failure})
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
    (if-let [v (input/input-read input)]
      (if (some #{v} s)
        {:input (input/input-advance input 1)
         :result v}
        {:input input
         :result :failure})
      {:input input
       :result :failure})))

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

(defn p-many1
  "This takes a parser, and runs it one or more times on the input, until it fails."
  [parser]
  (fn [input]
    {:pre [(:sequence input) (:position input)]}
    (let [{result :result input0 :input} (parser input)]
      (if (= :failure result)
        {:input input
         :result :failure}
        (reduce (fn [input parser]
                  (let [{result :result input1 :input} (parser (:input input))]
                    (if (= :failure result)
                      (reduced {:input input1
                                :result (:result input)})
                      {:input input1
                       :result (conj (or (:result input) [])
                                     result)})))
                {:input input0
                 :result [result]}
                (repeat parser))))))

(defn p-count
  "This takes a parser, and succeeds if it succeeds n times on the input."
  [n parser]
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
            (repeat n parser))))

(defn p-int
  "Parser that succeeds when the input is one or more base 10 digits (0-9)."
  []
  (fn [input]
    ((p-many1 (p-oneof [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9]))
     input)))

(defn p-parens
  "This succeeds when the input contains an opening parenthesis, followed
   by an input on which the supplied parser succeeds, followed by a closing
   parenthesis."
  [parser]
  (fn [input]
    (let [{result :result input0 :input} ((p-and (lit \() parser (lit \))) input)]
      (if (= :failure result)
        {:input input
         :result :failure}
        {:input input0
         :result [(:result (parser (:input ((lit \() input))))]}))))



(defn p-or
  [& parsers]
  (fn parser [input]
    {:pre [(:sequence input) (:position input)]}
    (reduce (fn [input parser]
              (let [{result :result input0 :input} (parser (:input input))]
                (if (not= :failure result)
                  (reduced {:input input0
                            :result result})
                  {:input (:input input)
                   :result :failure})))
            {:input input}
            parsers)))

(defn p-whitespace
  "Matches any whitespace character"
  []
  (fn [input]
    ((p-oneof [\space \tab \newline \formfeed \return \u000B])
     input)))

(defn p-whitespaces
  "Matches zero or more whitespaces"
  []
  (fn [input]
    ((p-many (p-whitespace))
     input)))

(defn p-tok
  "Matches the supplied parser and consumes any trailing whitespace"
  [parser]
  (fn [input]
    ((p-folding-and first parser (p-whitespaces))
     input)))

(defn p-string
  "Matches the string s exactly"
  [s]
  (fn [input]
    ((apply p-and (map lit s))
     input)))

(defn p-sym
  "Matches the string exactly and consumes any trailing whitespace"
  [s]
  (fn [input]
    ((p-tok (p-string s))
     input)))

(defn p-between
  "This succeeds when the input contains the opening string, followed by an
   input on which the supplied parser succeeds, followed by the closing string."
  [parser opening closing]
  (fn [input]
    (let [{result :result input0 :input} ((p-and (p-string opening)
                                                 parser
                                                 (p-string closing))
                                          input)]
      (if (= :failure result)
        {:input input
         :result :failure}
        {:input input0
         :result [(:result (parser (:input ((p-string opening) input))))]}))))

(defn p-range
  "Matches any single given character in the range s to e (inclusive)"
  [s e]
  (fn [input]
    ((apply p-or
            (map lit
                 (map char
                      (range (int s)
                             (inc (int e))))))
     input)))

(defn p-anchor
  "Consumes no input. Successful when function f returns true. Always returns a
  result of `nil`

  Function f is a anchor function. It takes as input the last character parsed,
  and the next character in the input, and returns success or failure. This
  function can be set by the user to ensure some condition is met. For example
  to test that the input is at a boundary between words and non-words.

  At the start of the input the first argument is set to nil, and at the end of
  the input the second argument is set to nil."
  [f]
  (fn [input]
    (if (f (input/input-read (update-in input [:position] dec))
           (input/input-read input))
      {:input input
       :result nil}
      {:input input
       :result :failure})))

(defn f-boundary-anchor
  "An anchor function that returns true when the previous character and next
  character are either side of a word boundary."
  [prev-char next-char]
  (let [word-chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
        prev-in-word (some #{prev-char} word-chars)
        next-in-word (some #{next-char} word-chars)]
    (or (and next-in-word (nil? prev-char))
        (and prev-in-word (nil? next-char))
        (and next-in-word (not prev-in-word))
        (and prev-in-word (not next-in-word)))))

(defn p-boundary
  "Matches only the boundary between words, and returns a result of `nil`"
  []
  (fn [input]
    ((p-anchor f-boundary-anchor)
     input)))

(defn p-soi
  "Parser that succeeds on the start of the input, and fails otherwise"
  []
  (fn [input]
    ((p-anchor (fn [prev-char _] (nil? prev-char)))
     input)))

(defn p-eoi
  "Parser that succeeds on the end of the input, and fails otherwise"
  []
  (fn [input]
    ((p-anchor (fn [_ next-char] (nil? next-char)))
     input)))

(defn p-whole
  "Parser that suceeds when the supplied parser succeeds on the whole input."
  [parser]
  (fn [input]
    ((p-and (p-soi) parser (p-eoi)) input)))
