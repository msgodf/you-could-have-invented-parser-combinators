(ns parser-combinators.core-test
  (:require [clojure.test :refer :all]
            [parser-combinators.core :refer :all]))

(deftest test-lit
  (testing "When the parser succeeds on the input, it returns the same character"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (lit \a)]
             (:result (parser input)))
           \a)))
  (testing "When the parser fails on the input, it returns failure"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (lit \b)]
             (:result (parser input)))
           :failure))))

(deftest test-or-combinator
  (testing "Takes two functions generated by lit, and returns the first one that succeeds"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-or (lit \a) (lit \z))]
             (:result (parser input)))
           \a))
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-or (lit \z) (lit \a))]
             (:result (parser input)))
           \a))))

(deftest test-and-combinator
  (testing "If both of them succeed it returns a vector of both characters"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-and (lit \a) (lit \b))]
             (:result (parser input)))
           [\a \b])))
  (testing "If both of them succeed it returns a vector of both integers"
    (is (= (let [input {:sequence [1 2 3 4]
                        :position 0}
                 parser (p-and (lit 1) (lit 2))]
             (:result (parser input)))
           [1 2])))
  (testing "If both of them succeed it advances the input position forward by two"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-and (lit \a) (lit \b))]
             (get-in (parser input) [:input :position]))
           2)))
  (testing "If any of them fail, it fails and rewinds the input."
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-and (lit \b) (lit \a))]
             (:result (parser input)))
           :failure))
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-and (lit \b) (lit \a))]
             (get-in (parser input) [:input :position]))
           0))))

(deftest test-apply
  (testing "Check for either ab or cd at the start of the input, and if either one is present convert the characters to integers and return a list of them"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (p-or (p-and (p-apply int (lit \a))
                                     (p-apply int (lit \b)))
                              (p-and (p-apply int (lit \c))
                                     (p-apply int (lit \d))))]

             (:result (parser input)))
           [97 98]))))

#_(run-tests)
