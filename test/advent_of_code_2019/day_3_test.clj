(ns advent-of-code-2019.day-3-test
  (:require [advent-of-code-2019.day-3 :refer :all]
            [clojure.test :refer :all]))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 6   (solution-part-one "R8,U5,L5,D3\nU7,R6,D4,L4")))
  (is (= 159 (solution-part-one "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83")))
  (is (= 135 (solution-part-one "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))))

