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

#_(run-tests)
