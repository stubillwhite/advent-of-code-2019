(ns advent-of-code-2019.day-6-test
  (:require [advent-of-code-2019.day-6 :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(def- example-input-part-one
  (string/join "\n" ["COM)B"
                     "B)C"
                     "C)D"
                     "D)E"
                     "E)F"
                     "B)G"
                     "G)H"
                     "D)I"
                     "E)J"
                     "J)K"
                     "K)L"]))

(def- example-input-part-two
  (string/join "\n" ["COM)B"
                     "B)C"
                     "C)D"
                     "D)E"
                     "E)F"
                     "B)G"
                     "G)H"
                     "D)I"
                     "E)J"
                     "J)K"
                     "K)L"
                     "K)YOU"
                     "I)SAN"]))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 42 (solution-part-one example-input-part-one))))

(deftest solution-part-two-given-example-input-then-example-result
  (is (= 4 (solution-part-two example-input-part-two))))
