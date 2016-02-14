(ns parser-combinators.parsers-test
  (:require [parser-combinators.parsers :as parsers]
            [clojure.test :refer :all]))

(deftest test-p-any-parser
  (testing "Matches any individual character"
    (is (= (-> {:sequence (seq "abcd")
                :position 0}
               ((parsers/p-any))
               (:result))
           \a))))

(deftest test-p-oneof-parser
  (testing "Matches any single given character in the string s"
    (is (= (-> {:sequence (seq "abcd")
                :position 0}
               ((parsers/p-oneof "ab"))
               (:result))
           \a))
    (is (= (-> {:sequence (seq "abcd")
                :position 0}
               ((parsers/p-oneof "ef"))
               (:result))
           :failure))))

(deftest test-and-combinator
  (testing "If both of them succeed it returns a vector of both characters"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-and (parsers/lit \a) (parsers/lit \b))]
             (:result (parser input)))
           [\a \b])))
  (testing "If both of them succeed it returns a vector of both integers"
    (is (= (let [input {:sequence [1 2 3 4]
                        :position 0}
                 parser (parsers/p-and (parsers/lit 1) (parsers/lit 2))]
             (:result (parser input)))
           [1 2])))
  (testing "If all of them succeed it returns a vector of all characters"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-and (parsers/lit \a)
                                       (parsers/lit \b)
                                       (parsers/lit \c)
                                       (parsers/lit \d))]
             (:result (parser input)))
           [\a \b \c \d])))
  (testing "If both of them succeed it advances the input position forward by two"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-and (parsers/lit \a) (parsers/lit \b))]
             (get-in (parser input) [:input :position]))
           2)))
  (testing "If any of them fail, it fails and rewinds the input."
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-and (parsers/lit \b) (parsers/lit \a))]
             (:result (parser input)))
           :failure))
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-and (parsers/lit \b) (parsers/lit \a))]
             (get-in (parser input) [:input :position]))
           0))))

(deftest test-folding-and
  (testing "An expression that matches the binary operator is calculated using a fold function"
    (= (let [math-fold (fn [[x op y]] (let [x (Integer/parseInt (str x))
                                            y (Integer/parseInt (str y))]
                                        (case op
                                          \* (* x y)
                                          \/ (/ x y)
                                          \+ (+ x y)
                                          \- (- x y)
                                          :failure)))]
         (:result ((parsers/p-folding-and
                    (parsers/lit \2) (parsers/p-oneof "*/+-") (parsers/lit \3))
                   {:sequence (seq "2*3")
                    :position 0})))
       6)))

#_(run-tests)
