(ns advent-of-code-2019.day-13-test
  (:require [advent-of-code-2019.day-13 :refer :all]
            [clojure.test :refer :all]))

(deftest solution-part-one-given-problem-input-then-problem-result
  (is (= 226 (solution-part-one problem-input))))

