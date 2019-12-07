(ns advent-of-code-2019.day-2-test
  (:require [advent-of-code-2019.day-2 :refer :all]
            [clojure.test :refer :all]))

(deftest solution-part-one-given-problem-input-then-problem-result
  (is (= 2782414 (solution-part-one))))

(deftest solution-part-two-given-problem-input-then-problem-result
  (is (= 9820 (solution-part-two))))
