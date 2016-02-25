(ns parser-combinators.parsers-test
  (:require [parser-combinators.parsers :as parsers]
            [parser-combinators.input :as input]
            [clojure.test :refer :all]))

(deftest test-p-any-parser
  (testing "Matches any individual character"
    (is (= (-> (input/input "abcd")
               ((parsers/p-any))
               (:result))
           \a))))

(deftest test-p-oneof-parser
  (testing "Matches any single given character in the string s"
    (is (= (-> (input/input "abcd")
               ((parsers/p-oneof "ab"))
               (:result))
           \a))
    (is (= (-> (input/input "abcd")
               ((parsers/p-oneof "ef"))
               (:result))
           :failure))))

(deftest test-and-combinator
  (testing "If both of them succeed it returns a vector of both characters"
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-and (parsers/lit \a) (parsers/lit \b))]
             (:result (parser input)))
           [\a \b])))
  (testing "If both of them succeed it returns a vector of both integers"
    (is (= (let [input (input/input [1 2 3 4])
                 parser (parsers/p-and (parsers/lit 1) (parsers/lit 2))]
             (:result (parser input)))
           [1 2])))
  (testing "If all of them succeed it returns a vector of all characters"
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-and (parsers/lit \a)
                                       (parsers/lit \b)
                                       (parsers/lit \c)
                                       (parsers/lit \d))]
             (:result (parser input)))
           [\a \b \c \d])))
  (testing "If both of them succeed it advances the input position forward by two"
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-and (parsers/lit \a) (parsers/lit \b))]
             (get-in (parser input) [:input :position]))
           2)))
  (testing "If any of them fail, it fails and rewinds the input."
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-and (parsers/lit \b) (parsers/lit \a))]
             (:result (parser input)))
           :failure))
    (is (= (let [input (input/input "abcd")
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
                       (input/input "2*3"))))
           6))))

(deftest test-many
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     (input/input "aaab")))
           [\a \a \a]))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    (input/input "aaab"))
                   [:input :position])
           3)))
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     (input/input "b")))
           []))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    (input/input "b"))
                   [:input :position])
           0)))
  (testing "Runs a zero or more times until it fails."
    (is (= (:result ((parsers/p-many (parsers/lit \a))
                     (input/input "")))
           []))
    (is (= (get-in ((parsers/p-many (parsers/lit \a))
                    (input/input ""))
                   [:input :position])
           0))))

(deftest test-many1
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     (input/input "aaab")))
           [\a \a \a]))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    (input/input "aaab"))
                   [:input :position])
           3)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     (input/input "ab")))
           [\a]))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    (input/input "ab"))
                   [:input :position])
           1)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     (input/input "b")))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    (input/input "b"))
                   [:input :position])
           0)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/lit \a))
                     (input/input "")))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/lit \a))
                    (input/input ""))
                   [:input :position])
           0)))
  (testing "Runs a one or more times until it fails."
    (is (= (:result ((parsers/p-many1 (parsers/p-oneof [\a]))
                     (input/input "")))
           :failure))
    (is (= (get-in ((parsers/p-many1 (parsers/p-oneof [\a]))
                    (input/input ""))
                   [:input :position])
           0))))

(deftest test-soi
  (testing "Succeed on the start of the input"
    (is (= (:result ((parsers/p-soi)
                     (input/input "abab")))
           nil)))
  (testing "Fail when not at the start of the input"
    (is (= (:result ((parsers/p-soi)
                     {:sequence "abab"
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
                     {:sequence "abab"
                      :position 3}))
           :failure)))
  (testing "Suceed at the start of an empty input"
    (is (= (:result ((parsers/p-eoi)
                     {:sequence ""
                      :position 0}))
           nil))))

(deftest test-whole
  (testing "Succeed when the parser succeeds at the whole input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     (input/input "ab")))
           [nil [\a \b] nil])))
  (testing "Fail when the parser only succeeds at the start of the input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     (input/input "abc")))
           :failure)))
  (testing "Fail when the parser only succeeds at the end of the input"
    (is (= (:result ((parsers/p-whole (parsers/p-and (parsers/lit \a)
                                                     (parsers/lit \b)))
                     {:sequence "abc"
                      :position 1}))
           :failure))))

(deftest test-or-combinator
  (testing "Takes a variable number of parsers, and returns the result of the first one that succeeds"
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (:result (parser input)))
           \a))
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-or (parsers/lit \z) (parsers/lit \a))]
             (:result (parser input)))
           \a))
    (is (= (let [input (input/input "abcd")
                 parser (parsers/p-or (parsers/lit \z)
                                      (parsers/lit \a)
                                      (parsers/lit \b))]
             (:result (parser input)))
           \a)))
  (testing "Takes a variable number of parsers, if none of them succeed, then fails, and the input is unchanged"
    (is (= (let [input (input/input "bcd")
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (:result (parser input)))
           :failure))
    (is (= (let [input (input/input "bcd")
                 parser (parsers/p-or (parsers/lit \a) (parsers/lit \z))]
             (get-in (parser input)
                     [:input :position]))
           0))))

(deftest test-int
  (testing "Succeeds on multiple digits"
    (is (= (:result ((parsers/p-int) (input/input "123")))
           [\1 \2 \3])))
  (testing "Succeeds on a single digit"
    (is (= (:result ((parsers/p-int) (input/input "0")))
           [\0])))
  (testing "Succeeds on a single digit followed by a non-digit"
    (is (= (:result ((parsers/p-int) (input/input "0.")))
           [\0])))
  (testing "Succeeds on multiple digits followed by a non-digit"
    (is (= (:result ((parsers/p-int) (input/input "123.")))
           [\1 \2 \3])))
  (testing "Fails when no digits are present"
    (is (= (:result ((parsers/p-int) (input/input "abcd")))
           :failure))))

(deftest test-parens
  (testing "Succeeds on a parenthesised input"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     (input/input "(a)")))
           [\a])))
  (testing "Succeeds on a nested parenthesised input"
    (is (= (:result ((parsers/p-parens (parsers/p-parens (parsers/lit \a)))
                     (input/input "((a))")))
           [[\a]])))
  (testing "Fails when input doesn't start with an opening parenthesis"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     (input/input "a)")))
           :failure)))
  (testing "Fails when input doesn't end with a closing parenthesis"
    (is (= (:result ((parsers/p-parens (parsers/lit \a))
                     (input/input "(a")))
           :failure))))

(deftest test-count-combinator
  (testing "If the parser succeeds n times, it returns a sequence of n results"
    (is (= (let [input (input/input "aaab")
                 parser (parsers/p-count 3 (parsers/lit \a))]
             (:result (parser input)))
           [\a \a \a])))
  (testing "If the count is zero, then it succeeds with an empty sequence of results"
    (is (= (let [input (input/input "b")
                 parser (parsers/p-count 0  (parsers/lit \a))]
             (:result (parser input)))
           [])))
  (testing "If the parser succeeds, but too few times, then it fails"
    (is (= (let [input (input/input "aab")
                 parser (parsers/p-count 3 (parsers/lit \a))]
             (:result (parser input)))
           :failure)))
  (testing "If the input is empty, then it fails"
    (is (= (let [input (input/input "")
                 parser (parsers/p-count 1 (parsers/lit \a))]
             (:result (parser input)))
           :failure))))

(deftest test-whitespace
  (testing "Succeeds on a space"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input " ")))
           \space)))
  (testing "Succeeds on a newline"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input "\n")))
           \newline)))
  (testing "Succeeds on a return"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input "\r")))
           \return)))
  (testing "Succeeds on a tab"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input "\t")))
           \tab)))
  (testing "Succeeds on a vertical tab (0x0B)"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input [\u000B])))
           \u000B)))
  (testing "Succeeds on a formfeed"
    (is (= (:result ((parsers/p-whitespace)
                     (input/input "\f")))
           \formfeed))))

(deftest test-whitespaces
  (testing "Succeeds on a mixture of whitespace characters"
    (is (= (:result ((parsers/p-whitespaces)
                     (input/input "\n\r \t\f \u000B")))
           [\newline \return \space \tab \formfeed \space \u000B])))
  (testing "Succeeds on a mixture of whitespace characters followed by a non-whitespace character"
    (is (= (:result ((parsers/p-whitespaces)
                     (input/input "\n\r \t\f \u000B!")))
           [\newline \return \space \tab \formfeed \space \u000B]))))

(deftest test-tok
  (testing "Succeeds on a parser followed by a mixture of whitespace characters, and the result only contains the token"
    (is (= (:result ((parsers/p-tok (parsers/p-and (parsers/lit \a)
                                                   (parsers/lit \b)
                                                   (parsers/lit \c)))
                     (input/input "abc\n\r \t\f \u000B")))
           [\a \b \c]))))

(deftest test-string
  (testing "Succeeds when the string matches exactly"
    (is (= (:result ((parsers/p-string "abc")
                     (input/input "abc")))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly"
    (is (= (:result ((parsers/p-string "abc")
                     (input/input "abcd")))
           [\a \b \c])))
  (testing "Fails when the string doesn't match"
    (is (= (:result ((parsers/p-string "abc")
                     (input/input "abdef")))
           :failure))))

(deftest test-sym
  (testing "Succeeds when the string matches exactly"
    (is (= (:result ((parsers/p-sym "abc")
                     (input/input "abc")))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly, and there is trailing whitespace, and the result only contains the string."
    (is (= (:result ((parsers/p-sym "abc")
                     (input/input "abc \t \n")))
           [\a \b \c])))
  (testing "Succeeds when the string matches the beginning of the input exactly, and there is trailing whitespace then more non-whitespace, and the result only contains the string."
    (is (= (:result ((parsers/p-sym "abc")
                     (input/input "abc \t \ndef")))
           [\a \b \c])))
  (testing "Fails when the string doesn't match"
    (is (= (:result ((parsers/p-sym "abc")
                     (input/input "abd")))
           :failure)))
  (testing "Fails when the string doesn't match, and there is trailing whitespace"
    (is (= (:result ((parsers/p-sym "abc")
                     (input/input "abd  \ndef")))
           :failure))))

(deftest test-between
  (testing "Succeeds when opening and closing strings are empty"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "" "")
                     (input/input "a")))
           [\a])))
  (testing "Succeeds when opening and closing strings are single characters"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     (input/input "oac")))
           [\a])))
  (testing "Succeeds when opening and closing strings are multiple characters"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "open" "close")
                     (input/input "openaclose")))
           [\a])))
  (testing "Succeeds on a nested input"
    (is (= (:result ((parsers/p-between (parsers/p-between (parsers/lit \a) "[" "]") "<" ">")
                     (input/input "<[a]>")))
           [[\a]])))
  (testing "Fails when input doesn't start with the opening string"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     (input/input "ac")))
           :failure)))
  (testing "Fails when input doesn't end with the closing string"
    (is (= (:result ((parsers/p-between (parsers/lit \a) "o" "c")
                     (input/input "oa")))
           :failure))))

(deftest test-range
  (testing "Succeeds when the range has the same start and end character"
    (is (= (:result ((parsers/p-range \a \a)
                     (input/input "a")))
           \a)))
  (testing "Succeeds when the range has a start and end character with no characters between"
    (is (= (:result ((parsers/p-range \a \b)
                     (input/input "a")))
           \a)))
  (testing "Succeeds when the range has a start and end character with a single character between"
    (is (= (:result ((parsers/p-range \a \c)
                     (input/input "a")))
           \a)))
  (testing "Succeeds when the middle of the range is matched"
    (is (= (:result ((parsers/p-range \a \c)
                     (input/input "b")))
           \b)))
  (testing "Succeeds when the end of the range is matched"
    (is (= (:result ((parsers/p-range \a \c)
                     (input/input "c")))
           \c)))
  (testing "Fails on an input character before the start of the range"
    (is (= (:result ((parsers/p-range \b \c)
                     (input/input "a")))
           :failure)))
  (testing "Fails on an input character after the end of the range"
    (is (= (:result ((parsers/p-range \b \c)
                     (input/input "d")))
           :failure))))

(deftest test-anchor
  (testing "Consumes no input when anchor function returns true"
    (is (zero? (-> (input/input "abc")
                   ((parsers/p-anchor (constantly true)))
                   :input
                   :position))))
  (testing "Consumes no input when anchor function returns false"
    (is (zero? (-> (input/input "abc")
                   ((parsers/p-anchor (constantly false)))
                   :input
                   :position))))
  (testing "Succeeds with result of `nil` when anchor function returns true"
    (is (nil? (-> (input/input "abc")
                  ((parsers/p-anchor (constantly true)))
                  :result))))
  (testing "Fails when anchor function returns false"
    (is (= (-> (input/input "abc")
               ((parsers/p-anchor (constantly false)))
               :result)
           :failure)))
  (testing "Calls the anchor function with a first argument of `nil` when at the start of the input"
    (is (and (= (-> (input/input "abc")
                    ((parsers/p-anchor (constantly false)))
                    :result)
                :failure)
             (= (-> (input/input "abc")
                    ((parsers/p-anchor (fn [a b] (not (nil? a)))))
                    :result)
                :failure))))
  (testing "Calls the anchor function with a second argument of `nil` when at the end of the input"
    (is (and (= (-> (input/input "abc")
                    ((parsers/p-anchor (constantly false)))
                    :result)
                :failure)
             (= (-> {:sequence "abc" :position 3}
                    ((parsers/p-anchor (fn [a b] (not (nil? b)))))
                    :result)
                :failure)))))

(deftest test-boundary-anchor-function
  (testing "Boundary anchor function returns true when one input is nil"
    (is (parsers/f-boundary-anchor nil \a))
    (is (parsers/f-boundary-anchor \a nil)))
  (testing "Boundary anchor function returns true when one input is non-word character"
    (is (parsers/f-boundary-anchor \a \space))
    (is (parsers/f-boundary-anchor \space \a)))
  (testing "Boundary anchor function returns false when both inputs are word characters"
    (is (not (parsers/f-boundary-anchor \a \b)))))


(deftest test-boundary
  (testing "Succeeds on word boundary, with a result of `nil`"
    (is (nil? (:result ((parsers/p-boundary)
                        {:sequence "ab cd"
                         :position 2})))))
  (testing "Succeeds on start of input, with a result of `nil`"
    (is (nil? (:result ((parsers/p-boundary)
                        {:sequence "ab"
                         :position 0})))))
  (testing "Succeeds on end of input, with a result of `nil`"
    (is (nil? (:result ((parsers/p-boundary)
                        {:sequence "ab"
                         :position 2})))))
  (testing "Fails on non boundary"
    (is (= (:result ((parsers/p-boundary)
                     {:sequence "ab"
                      :position 1}))
           :failure))))

(deftest test-startswith
  (testing "Matches an empty string"
    (is (= (:result ((parsers/p-startswith (parsers/p-eoi))
                     (input/input "")))
           [nil nil])))
  (testing "Matches a single character at the start"
    (is (= (:result ((parsers/p-startswith (parsers/lit \a))
                     (input/input "a")))
           [nil \a])))
  (testing "Matches a single character at the start, followed by more input"
    (is (= (:result ((parsers/p-startswith (parsers/lit \a))
                     (input/input "ab")))
           [nil \a])))
  (testing "Fails when the parser doesn't match the start of the input"
    (is (= (:result ((parsers/p-startswith (parsers/lit \a))
                     (input/input "b")))
           :failure))))

(deftest test-endswith
  (testing "Matches an empty string"
    (is (= (:result ((parsers/p-endswith (parsers/p-soi))
                     (input/input "")))
           [nil nil])))
  (testing "Matches a single character at the end"
    (is (= (:result ((parsers/p-endswith (parsers/lit \a))
                     (input/input "a")))
           [\a nil])))
  (testing "Fails when the parser doesn't match the end of the input"
    (is (= (:result ((parsers/p-endswith (parsers/lit \a))
                     (input/input "ab")))
           :failure))))

(deftest test-maybe
  (testing "Succeeds when the parser succeeds"
    (is (= (:result ((parsers/p-maybe (parsers/lit \a))
                     (input/input "a")))
           \a)))
  (testing "Succeeds when the parser fails, with a result of nil"
    (is (nil? (:result ((parsers/p-maybe (parsers/lit \a))
                        (input/input "b")))))))

#_(run-tests)
