(ns advent-of-code-2019.day-1-test
  (:require [advent-of-code-2019.day-1 :refer :all]
            [clojure.test :refer :all]
            [clojure.string :as string]))

(defn- test-input [masses]
  (string/join "\n" masses))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 2     (solution-part-one (test-input [12]))))
  (is (= 2     (solution-part-one (test-input [14]))))
  (is (= 654   (solution-part-one (test-input [1969]))))
  (is (= 33583 (solution-part-one (test-input [100756])))))

(deftest solution-part-two-given-example-input-then-example-result
  (is (= 2     (solution-part-two (test-input [14]))))
  (is (= 966   (solution-part-two (test-input [1969]))))
  (is (= 50346 (solution-part-two (test-input [100756])))))
