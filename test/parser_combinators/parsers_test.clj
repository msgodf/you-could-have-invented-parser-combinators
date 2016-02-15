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
    (is (= (let [math-fold (fn [[x op y]] (let [x (Integer/parseInt (str x))
                                                y (Integer/parseInt (str y))]
                                            (case op
                                              \* (* x y)
                                              \/ (/ x y)
                                              \+ (+ x y)
                                              \- (- x y)
                                              :failure)))]
             (:result ((parsers/p-folding-and math-fold
                          (parsers/lit \2) (parsers/p-oneof "*/+-") (parsers/lit \3))
                         {:sequence (seq "2*3")
                          :position 0})))
           6))))

(deftest test-many
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     {:sequence "aaab"
                      :position 0}))
           [\a \a \a]))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    {:sequence "aaab"
                     :position 0})
                   [:input :position])
           3)))
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     {:sequence "b"
                      :position 0}))
           []))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    {:sequence "b"
                     :position 0})
                   [:input :position])
           0)))
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     {:sequence ""
                      :position 0}))
           []))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    {:sequence ""
                     :position 0})
                   [:input :position])
           0))))

(deftest test-many1
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     {:sequence "aaab"
                      :position 0}))
           [\a \a \a]))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    {:sequence "aaab"
                     :position 0})
                   [:input :position])
           3)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     {:sequence "ab"
                      :position 0}))
           [\a]))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    {:sequence "ab"
                     :position 0})
                   [:input :position])
           1)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     {:sequence "b"
                      :position 0}))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    {:sequence "b"
                     :position 0})
                   [:input :position])
           0)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     {:sequence ""
                      :position 0}))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    {:sequence ""
                     :position 0})
                   [:input :position])
           0)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/p-oneof [\a]))
                     {:sequence ""
                      :position 0}))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/p-oneof [\a]))
                    {:sequence ""
                     :position 0})
                   [:input :position])
           0))))
(deftest test-soi
  (testing "Succeed on the start of the input"
    (is (= (:result ((parsers/p-soi)
                     {:sequence "abab"
                      :position 0}))
           nil)))
  (testing "Fail when not at the start of the input"
    (is (= (:result ((parsers/p-soi)
                     {:sequnce "abab"
                      :position 1}))
           :failure)))
  (testing "Fail when not at the start of the input, even for an empty input"
    (is (= (:result ((parsers/p-soi)
                     {:sequnce ""
                      :position 1}))
           :failure))))

(deftest test-eoi
  (testing "Succeed on the end of the input"
    (is (= (:result ((parsers/p-eoi)
                     {:sequence "abab"
                      :position 4}))
           nil)))
  (testing "Fail when not at the end of the input"
    (is (= (:result ((parsers/p-eoi)
                     {:sequnce "abab"
                      :position 3}))
           :failure)))
  (testing "Suceed at the start of an empty input"
    (is (= (:result ((parsers/p-eoi)
                     {:sequnce ""
                      :position 0}))
           nil))))

(deftest test-whole
  (testing "Succeed when the parser succeeds at the whole input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     {:sequence "ab"
                      :position 0}))
           [nil [\a \b] nil])))
  (testing "Fail when the parser only succeeds at the start of the input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     {:sequence "abc"
                      :position 0}))
           :failure)))
  (testing "Fail when the parser only succeeds at the end of the input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     {:sequence "abc"
                      :position 1}))
           :failure))))

(deftest test-or-combinator
  (testing "Takes a variable number of parsers, and returns the result of the first one that succeeds"
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (:result (parser input)))
           \a))
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-or (parsers/lit \z) (parsers/lit \a))]
             (:result (parser input)))
           \a))
    (is (= (let [input {:sequence (seq "abcd")
                        :position 0}
                 parser (parsers/p-or (parsers/lit \z)
                                      (parsers/lit \a)
                                      (parsers/lit \b))]
             (:result (parser input)))
           \a)))
  (testing "Takes a variable number of parsers, if none of them succeed, then fails, and the input is unchanged"
    (is (= (let [input {:sequence (seq "bcd")
                        :position 0}
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (:result (parser input)))
           :failure))
    (is (= (let [input {:sequence (seq "bcd")
                        :position 0}
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (get-in (parser input)
                     [:input :position]))
           0))))

#_(run-tests)
