(ns advent-of-code-2019.computer-test
  (:require [advent-of-code-2019.computer :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

;; Basic functions

(deftest position-addressing-mode
  (let [prg (into [] (range 10 15))]
    (testing "read-value given offset within program then returns value at offset"
      (is (= 10 (read-value 0 prg 0)))
      (is (= 12 (read-value 0 prg 2)))
      (is (= 13 (read-value 0 prg 3))))
    (testing "write-value given offset within program then writes value at offset"
      (is (= [50 11 12 13 14] (write-value 0 prg 0 50)))
      (is (= [10 11 50 13 14] (write-value 0 prg 2 50)))
      (is (= [10 11 12 50 14] (write-value 0 prg 3 50))))))

(deftest immediate-addressing-mode
  (let [prg (into [] (range 10 15))]
    (testing "read-value given literal then returns literal"
      (is (= 0 (read-value 1 prg 0)))
      (is (= 2 (read-value 1 prg 2)))
      (is (= 3 (read-value 1 prg 3))))
    (testing "write-value given literal then writes value at offset of literal"
      (is (= [50 11 12 13 14] (write-value 1 prg 0 50)))
      (is (= [10 11 50 13 14] (write-value 1 prg 2 50)))
      (is (= [10 11 12 50 14] (write-value 1 prg 3 50))))))

;; Example programs

(defn- create-program [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn- create-computer
  ([prg]
   (create-computer prg []))
  ([prg stdin]
   (initialise-computer (create-program prg) stdin)))

(defn- prg-of [cmp]
  (string/join "," (:prg cmp)))

(defn- stdout-of [cmp]
  (string/join "," (:stdout cmp)))

(defn- execute-and-get-prg [prg]
  (->> (create-computer prg)
       (execute-program)
       (prg-of)))

(deftest step-program-given-day-two-example-input-then-example-result
  (testing "stepping example program"
    (is (= [{:ip 0 :prg [1 9 10 3 2 3 11 0 99 30 40 50]}
            {:ip 4 :prg [1 9 10 70 2 3 11 0 99 30 40 50]}
            {:ip 8 :prg [3500 9 10 70 2 3 11 0 99 30 40 50]}]
           (->> (create-computer "1,9,10,3,2,3,11,0,99,30,40,50")
                (step-program)
                (map (fn [x] (select-keys x [:ip :prg]))))))))

(deftest step-program-given-day-two-example-input-then-example-result
  (testing "final state of example prgrams"
    (is (= "2,0,0,0,99"          (execute-and-get-prg "1,0,0,0,99")))
    (is (= "2,3,0,6,99"          (execute-and-get-prg "2,3,0,3,99")))
    (is (= "2,4,4,5,99,9801"     (execute-and-get-prg "2,4,4,5,99,0")))
    (is (= "30,1,1,4,2,5,6,0,99" (execute-and-get-prg "1,1,1,4,99,5,6,0,99")))))

(deftest step-program-given-day-five-example-input-then-example-result
  (testing "final state of example prgrams"
    (is (= "1002,4,3,4,99" (execute-and-get-prg "1002,4,3,4,33")))))

(deftest execute-program-given-day-five-example-input-then-example-final-state
  (let [final-state (execute-program (create-computer "3,0,4,0,99" [23]))]
    (is (= "23,0,4,0,99" (prg-of final-state)))
    (is (= "23"          (stdout-of final-state)))))
