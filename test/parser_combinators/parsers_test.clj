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
                     {:sequence "abab"
                      :position 1}))
           :failure)))
  (testing "Fail when not at the start of the input, even for an empty input"
    (is (= (:result ((parsers/p-soi)
                     {:sequence ""
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

(deftest test-int
  (testing "Succeeds on multiple digits"
    (is (= (:result ((parsers/p-int) {:sequence "123"
                                      :position 0}))
           [\1 \2 \3])))
  (testing "Succeeds on a single digit"
    (is (= (:result ((parsers/p-int) {:sequence "0"
                                      :position 0}))
           [\0])))
  (testing "Succeeds on a single digit followed by a non-digit"
    (is (= (:result ((parsers/p-int) {:sequence "0."
                                      :position 0}))
           [\0])))
  (testing "Succeeds on multiple digits followed by a non-digit"
    (is (= (:result ((parsers/p-int) {:sequence "123."
                                      :position 0}))
           [\1 \2 \3])))
  (testing "Fails when no digits are present"
    (is (= (:result ((parsers/p-int) {:sequence "abcd"
                                      :position 0}))
           :failure))))

(deftest test-parens
  (testing "Succeeds on a parenthesised input"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     {:sequence "(a)"
                      :position 0}))
           [\a])))
  (testing "Succeeds on a nested parenthesised input"
    (is (= (:result ((parsers/p-parens (parsers/p-parens (parsers/lit \a)))
                     {:sequence "((a))"
                      :position 0}))
           [[\a]])))
  (testing "Fails when input doesn't start with an opening parenthesis"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     {:sequence "a)"
                      :position 0}))
           :failure)))
  (testing "Fails when input doesn't end with a closing parenthesis"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     {:sequence "(a"
                      :position 0}))
           :failure))))

(deftest test-count-combinator
  (testing "If the parser succeeds n times, it returns a sequence of n results"
    (is (= (let [input {:sequence (seq "aaab")
                        :position 0}
                 parser (parsers/p-count 3 (parsers/lit \a))]
             (:result (parser input)))
           [\a \a \a])))
  (testing "If the count is zero, then it succeeds with an empty sequence of results"
    (is (= (let [input {:sequence (seq "b")
                        :position 0}
                 parser (parsers/p-count 0  (parsers/lit \a))]
             (:result (parser input)))
           [])))
  (testing "If the parser succeeds, but too few times, then it fails"
    (is (= (let [input {:sequence (seq "aab")
                        :position 0}
                 parser (parsers/p-count 3 (parsers/lit \a))]
             (:result (parser input)))
           :failure)))
  (testing "If the input is empty, then it fails"
    (is (= (let [input {:sequence ""
                        :position 0}
                 parser (parsers/p-count 1 (parsers/lit \a))]
             (:result (parser input)))
           :failure))))

(deftest test-whitespace
  (testing "Succeeds on a space"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence (seq " ")
                      :position 0}))
           \space)))
  (testing "Succeeds on a newline"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence (seq "\n")
                      :position 0}))
           \newline)))
  (testing "Succeeds on a return"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence (seq "\r")
                      :position 0}))
           \return)))
  (testing "Succeeds on a tab"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence (seq "\t")
                      :position 0}))
           \tab)))
  (testing "Succeeds on a vertical tab (0x0B)"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence [\u000B]
                      :position 0}))
           \u000B)))
  (testing "Succeeds on a formfeed"
    (is (= (:result ((parsers/p-whitespace)
                     {:sequence (seq "\f")
                      :position 0}))
           \formfeed))))

(deftest test-whitespaces
  (testing "Succeeds on a mixture of whitespace characters"
    (is (= (:result ((parsers/p-whitespaces)
                     {:sequence (seq "\n\r \t\f \u000B")
                      :position 0}))
           [\newline \return \space \tab \formfeed \space \u000B])))
  (testing "Succeeds on a mixture of whitespace characters followed by a non-whitespace character"
    (is (= (:result ((parsers/p-whitespaces)
                     {:sequence (seq "\n\r \t\f \u000B!")
                      :position 0}))
           [\newline \return \space \tab \formfeed \space \u000B]))))

(deftest test-tok
  (testing "Succeeds on a parser followed by a mixture of whitespace characters, and the result only contains the token"
    (is (= (:result ((parsers/p-tok (parsers/p-and (parsers/lit \a)
                                                   (parsers/lit \b)
                                                   (parsers/lit \c)))
                     {:sequence (seq "abc\n\r \t\f \u000B")
                      :position 0}))
           [\a \b \c]))))

(deftest test-string
  (testing "Succeeds when the string matches exactly"
    (is (= (:result ((parsers/p-string "abc")
                     {:sequence "abc"
                      :position 0}))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly"
    (is (= (:result ((parsers/p-string "abc")
                     {:sequence "abcd"
                      :position 0}))
           [\a \b \c])))
  (testing "Fails when the string doesn't match"
    (is (= (:result ((parsers/p-string "abc")
                     {:sequence "abdef"
                      :position 0}))
           :failure))))

(deftest test-sym
  (testing "Succeeds when the string matches exactly"
    (is (= (:result ((parsers/p-sym "abc")
                     {:sequence "abc"
                      :position 0}))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly, and there is trailing whitespace, and the result only contains the string."
    (is (= (:result ((parsers/p-sym "abc")
                     {:sequence "abc \t \n"
                      :position 0}))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly, and there is trailing whitespace then more non-whitespace, and the result only contains the string."
    (is (= (:result ((parsers/p-sym "abc")
                     {:sequence "abc \t \ndef"
                      :position 0}))
           [\a \b \c])))
  (testing "Fails when the string doesn't match"
    (is (= (:result ((parsers/p-sym "abc")
                     {:sequence "abd"
                      :position 0}))
           :failure)))
  (testing "Fails when the string doesn't match, and there is trailing whitespace"
    (is (= (:result ((parsers/p-sym "abc")
                     {:sequence "abd  \ndef"
                      :position 0}))
           :failure))))

(deftest test-between
  (testing "Succeeds when opening and closing strings are empty"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "" "")
                     {:sequence "a"
                      :position 0}))
           [\a])))
  (testing "Succeeds when opening and closing strings are single characters"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     {:sequence "oac"
                      :position 0}))
           [\a])))
  (testing "Succeeds when opening and closing strings are multiple characters"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "open" "close")
                     {:sequence "openaclose"
                      :position 0}))
           [\a])))
  (testing "Succeeds on a nested input"
    (is (= (:result ((parsers/p-between (parsers/p-between (parsers/lit \a) "[" "]") "<" ">")
                     {:sequence "<[a]>"
                      :position 0}))
           [[\a]])))
  (testing "Fails when input doesn't start with the opening string"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     {:sequence "ac"
                      :position 0}))
           :failure)))
  (testing "Fails when input doesn't end with the closing string"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     {:sequence "oa"
                      :position 0}))
           :failure))))

(deftest test-range
  (testing "Succeeds when the range has the same start and end character"
    (is (= (:result ((parsers/p-range \a \a)
                     {:sequence "a"
                      :position 0}))
           \a)))
  (testing "Succeeds when the range has a start and end character with no characters between"
    (is (= (:result ((parsers/p-range \a \b)
                     {:sequence "a"
                      :position 0}))
           \a)))
  (testing "Succeeds when the range has a start and end character with a single character between"
    (is (= (:result ((parsers/p-range \a \c)
                     {:sequence "a"
                      :position 0}))
           \a)))
  (testing "Succeeds when the middle of the range is matched"
    (is (= (:result ((parsers/p-range \a \c)
                     {:sequence "b"
                      :position 0}))
           \b)))
  (testing "Succeeds when the end of the range is matched"
    (is (= (:result ((parsers/p-range \a \c)
                     {:sequence "c"
                      :position 0}))
           \c)))
  (testing "Fails on an input character before the start of the range"
    (is (= (:result ((parsers/p-range \b \c)
                     {:sequence "a"
                      :position 0}))
           :failure)))
  (testing "Fails on an input character after the end of the range"
    (is (= (:result ((parsers/p-range \b \c)
                     {:sequence "d"
                      :position 0}))
           :failure))))

#_(run-tests)
