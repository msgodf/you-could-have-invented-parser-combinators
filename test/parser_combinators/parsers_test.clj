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

#_(run-tests)
