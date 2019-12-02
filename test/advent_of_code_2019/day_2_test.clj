(ns advent-of-code-2019.day-2-test
  (:require [advent-of-code-2019.day-2 :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(def- solution-one-example-output
  [{:ip 0 :prg [1 9 10 3 2 3 11 0 99 30 40 50] :halted? false}
   {:ip 4 :prg [1 9 10 70 2 3 11 0 99 30 40 50] :halted? false}
   {:ip 8 :prg [3500 9 10 70 2 3 11 0 99 30 40 50] :halted? false}])

(defn- execute-and-get-final-state-of [prg]
  (->> (step-program (parse-input prg))
       last
       :prg
       (string/join ",")))

(deftest solution-part-one-given-example-input-then-example-result
  (testing "stepping example program"
    (is (= solution-one-example-output (step-program (parse-input "1,9,10,3,2,3,11,0,99,30,40,50")))))
  (testing "final state of example prgrams"
    (is (= "2,0,0,0,99"          (execute-and-get-final-state-of "1,0,0,0,99")))
    (is (= "2,3,0,6,99"          (execute-and-get-final-state-of "2,3,0,3,99")))
    (is (= "2,4,4,5,99,9801"     (execute-and-get-final-state-of "2,4,4,5,99,0")))
    (is (= "30,1,1,4,2,5,6,0,99" (execute-and-get-final-state-of "1,1,1,4,99,5,6,0,99")))))



