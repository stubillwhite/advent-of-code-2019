(ns advent-of-code-2019.day-5-test
  (:require [advent-of-code-2019.day-5 :refer :all]
            [clojure.test :refer :all]))

(deftest solution-part-one-given-problem-input-then-problem-result
  (is (= 14155342 (last (solution-part-one)))))

(deftest solution-part-two-given-problem-input-then-problem-result
  (is (= 8684145 (last (solution-part-two)))))
