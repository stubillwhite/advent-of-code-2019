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
    (is (not (two-adjacent-characters? "abc"))))
  (testing "exactly-two-adjacent-characters?"
    (is (exactly-two-adjacent-characters? "aabc"))
    (is (exactly-two-adjacent-characters? "abbc"))
    (is (exactly-two-adjacent-characters? "abba"))
    (is (exactly-two-adjacent-characters? "aabbb"))
    (is (not (exactly-two-adjacent-characters? "aaab")))
    (is (not (exactly-two-adjacent-characters? "abbb")))))

(deftest valid-basic-password-given-example-input-then-example-result
  (is (= true  (valid-basic-password? "111111")))
  (is (= false (valid-basic-password? "223450")))
  (is (= false (valid-basic-password? "123789"))))

(deftest valid-strict-password-given-example-input-then-example-result
  (is (= true  (valid-strict-password? "112233")))
  (is (= false (valid-strict-password? "123444")))
  (is (= true  (valid-strict-password? "111122"))))
