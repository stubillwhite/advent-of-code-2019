(ns advent-of-code-2019.day-8-test
  (:require [advent-of-code-2019.day-8 :refer :all]
            [clojure.test :refer :all]))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 1 (solution-part-one "123456789012" 3 2))))
