(ns advent-of-code-2019.day-4-test
  (:require [advent-of-code-2019.day-4 :refer :all]
            [clojure.test :refer :all]))

(deftest password-criteria-predicates
  (testing "digits-increase?"
    (is (= true  (digits-increase? "111")))
    (is (= true  (digits-increase? "123")))
    (is (= false (digits-increase? "132"))))
  (testing "two-adjacent-characters?"
    (is (two-adjacent-characters? "aaa"))
    (is (two-adjacent-characters? "aab"))
    (is (two-adjacent-characters? "abb"))
    (is (not (two-adjacent-characters? "aba")))
    (is (not (two-adjacent-characters? "abc")))))

