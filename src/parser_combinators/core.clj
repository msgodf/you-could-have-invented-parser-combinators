(ns parser-combinators.core)

;; Based on the post about parser combinators, with examples in Javascript
;; http://theorangeduck.com/page/you-could-have-invented-parser-combinators

(defn input-read
  [input]
  (let [{position :position xs :sequence} input]
    (nth xs position)))

(defn input-advance
  [input n]
  (update-in input
             [:position]
             (fn [v] (+ v n))))

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

(defn p-and
  [parser0 parser1]
  (fn parser [input]
    (let [{result0 :result input0 :input} (parser0 input)]
      (if (= :failure result0)
        {:input input
         :result :failure}
        (let [{result1 :result input1 :input} (parser1 input0)]
          (if (= :failure result1)
            {:input input
             :result :failure}
            {:input input1
             :result [result0 result1]}))))))
