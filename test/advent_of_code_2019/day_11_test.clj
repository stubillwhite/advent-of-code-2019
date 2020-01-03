(ns advent-of-code-2019.day-11-test
  (:require [advent-of-code-2019.day-11 :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(def- example-output
  [1 0
   0 0
   1 0
   1 0
   0 1
   1 0
   1 0])

(defn- program-generating-output [output]
  (str
   (->> output
        (interleave (repeat 11104))
        (string/join ","))
   ",99"))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 6 (solution-part-one (program-generating-output example-output)))))
