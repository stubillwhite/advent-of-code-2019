(ns advent-of-code-2019.day-6-test
  (:require [advent-of-code-2019.day-6 :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(def- example-input (string/join "\n" ["COM)B"
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

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 42 (solution-part-one example-input))))


