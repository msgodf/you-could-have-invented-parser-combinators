(ns parser-combinators.math-test
  (:require [parser-combinators.math :as math]
            [clojure.test :refer :all]))

(deftest test-expression-evaluation
  (testing "Can evaluate a non-parenthesised multiplication"
    (is (= (:result ((math/p-math) {:sequence "2*3"
                                    :position 0}))
           [nil 6 nil])))
  (testing "Can evaluate a non-parenthesised addition"
    (is (= (:result ((math/p-math) {:sequence "2+3"
                                    :position 0}))
           [nil 5 nil])))
  (testing "Can evaluate a parenthesised multiplication"
    (is (= (:result ((math/p-math) {:sequence "(2*3)"
                                    :position 0}))
           [nil [6] nil])))
  (testing "Can evaluate a parenthesised addition"
    (is (= (:result ((math/p-math) {:sequence "(2+3)"
                                    :position 0}))
           [nil [5] nil])))
  (testing "Can evaluate a multiplication then an addition"
    (is (= (:result ((math/p-math) {:sequence "(2*3)+5"
                                    :position 0}))
           [nil 11 nil])))
  (testing "Can evaluate an addition then a multiplication"
    (is (= (:result ((math/p-math) {:sequence "5+(2*3)"
                                    :position 0}))
           [nil 11 nil])))
  (testing "Fail to evaluate a multiplication of three integers"
    (is (= (:result ((math/p-math) {:sequence "1*2*3"
                                    :position 0}))
           :failure)))
  (testing "Fail to evaluate an addition of three integers"
    (is (= (:result ((math/p-math) {:sequence "1+2+3"
                                    :position 0}))
           :failure))))

#_(run-tests)
