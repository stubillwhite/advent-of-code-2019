(ns advent-of-code-2019.computer-test
  (:require [advent-of-code-2019.computer :refer :all]
            [clojure.string :as string]
            [clojure.test :refer :all]))

;; Test helpers

(defn- to-csv [xs]
  (string/join "," xs))

(defn- from-csv [s]
  (if (string/blank? s)
    []
    (->> (string/split s #",")
         (map #(Long/parseLong %))
         (into []))))

(defn- create-computer
  ([prg]
   (create-computer prg ""))
  ([prg stdin]
   (-> (initialise-computer (from-csv prg))
       (assoc-in [:io :stdin] (from-csv stdin)))))

(defn- prg-of    [cmp] (to-csv (:prg cmp)))
(defn- stdin-of  [cmp] (to-csv (get-in cmp [:io :stdin])))
(defn- stdout-of [cmp] (to-csv (get-in cmp [:io :stdout])))

(defn- execute
  ([prg]
   (execute prg ""))
  ([prg stdin]
   (->> (create-computer prg stdin)
        (execute-program))))

(defn- execute-and-get-prg
  ([prg]
   (execute-and-get-prg prg ""))
  ([prg stdin]
   (->> (execute prg stdin)
        (prg-of))))

(defn- execute-and-get-stdout
  ([prg]
   (execute-and-get-stdout prg ""))
  ([prg stdin]
   (->> (execute prg stdin)
        (stdout-of))))

;; Instruction set

(deftest add-instruction
  (testing "add positional"
    (is (= "100,0,4,0,99" (execute-and-get-prg "00001,0,4,0,99"))))
  (testing "add immediate"
    (is (= "8,3,5,0,99" (execute-and-get-prg "11101,3,5,0,99"))))
  (testing "add relative"
    (is (= "9,1,22201,0,5,6,99,100" (execute-and-get-prg "9,1,22201,0,5,6,99")))))

(deftest multiply-instruction
  (testing "multiply positional"
    (is (= "198,0,4,0,99" (execute-and-get-prg "00002,0,4,0,99"))))
  (testing "multiply immediate"
    (is (= "15,3,5,0,99" (execute-and-get-prg "11102,3,5,0,99"))))
  (testing "multiply relative"
    (is (= "9,1,22202,-1,5,6,99,891" (execute-and-get-prg "9,1,22202,-1,5,6,99")))))

(deftest read-instruction
  (testing "read positional"
    (let [result (execute "00003,0,99" "1,2,3")]
      (is (= "1,0,99" (prg-of result)))
      (is (= "2,3"    (stdin-of result)))))
  (testing "read immediate"
    (let [result (execute "11103,0,99" "1,2,3")]
      (is (= "1,0,99" (prg-of result)))
      (is (= "2,3"    (stdin-of result)))))
  (testing "read relative"
    (let [result (execute "9,1,22203,-1,99" "1,2,3")]
      (is (= "1,1,22203,-1,99" (prg-of result)))
      (is (= "2,3"             (stdin-of result))))))

(deftest write-instruction
  (testing "write positional"
    (is (= "99" (stdout-of (execute "00004,2,99")))))
  (testing "write immediate"
    (is (= "23" (stdout-of (execute "11104,23,99")))))
  (testing "write relative"
    (is (= "9" (stdout-of (execute "9,1,22204,-1,99"))))))

(deftest jump-if-true-instruction
  (testing "jump-if-true positional"
    (is (= "23" (stdout-of (execute "00005,9,10,11104,23,99,11104,42,99,0,6"))))
    (is (= "42" (stdout-of (execute "00005,9,10,11104,23,99,11104,42,99,1,6")))))
  (testing "jump-if-true immediate"
    (is (= "23" (stdout-of (execute "11105,0,6,11104,23,99,11104,42,99"))))
    (is (= "42" (stdout-of (execute "11105,1,6,11104,23,99,11104,42,99")))))
  (testing "jump-if-true relative"
    (is (= "23" (stdout-of (execute "9,1,22205,10,11,11104,23,99,11104,42,99,0,8"))))
    (is (= "42" (stdout-of (execute "9,1,22205,10,11,11104,23,99,11104,42,99,1,8"))))))

(deftest jump-if-false-instruction
  (testing "jump-if-false positional"
    (is (= "42" (stdout-of (execute "00006,9,10,11104,23,99,11104,42,99,0,6"))))
    (is (= "23" (stdout-of (execute "00006,9,10,11104,23,99,11104,42,99,1,6")))))
  (testing "jump-if-false immediate"
    (is (= "42" (stdout-of (execute "11106,0,6,11104,23,99,11104,42,99"))))
    (is (= "23" (stdout-of (execute "11106,1,6,11104,23,99,11104,42,99")))))
  (testing "jump-if-false relative"
    (is (= "42" (stdout-of (execute "9,1,22206,10,11,11104,23,99,11104,42,99,0,8"))))
    (is (= "23" (stdout-of (execute "9,1,22206,10,11,11104,23,99,11104,42,99,1,8"))))))

(deftest less-than-instruction
  (testing "less-than positional"
    (is (= "7,1,2,1,99" (execute-and-get-prg "00007,1,2,3,99")))
    (is (= "7,3,2,0,99" (execute-and-get-prg "00007,3,2,3,99"))))
  (testing "less-than immediate"
    (is (= "11107,1,2,1,99" (execute-and-get-prg "11107,1,2,3,99")))
    (is (= "11107,2,1,0,99" (execute-and-get-prg "11107,2,1,3,99"))))
  (testing "less-than relative"
    (is (= "9,1,22207,2,3,1,99" (execute-and-get-prg "9,1,22207,2,3,4,99")))
    (is (= "9,1,22207,4,3,0,99" (execute-and-get-prg "9,1,22207,4,3,4,99")))))

(deftest equals-instruction
  (testing "equals positional"
    (is (= "8,5,6,1,99,23,23" (execute-and-get-prg "00008,5,6,3,99,23,23")))
    (is (= "8,5,6,0,99,23,42" (execute-and-get-prg "00008,5,6,3,99,23,42"))))
  (testing "equals immediate"
    (is (= "11108,1,1,1,99" (execute-and-get-prg "11108,1,1,3,99")))
    (is (= "11108,1,0,0,99" (execute-and-get-prg "11108,1,0,3,99"))))
  (testing "equals relative"
    (is (= "9,1,22208,6,7,1,99,23,23" (execute-and-get-prg "9,1,22208,6,7,4,99,23,23")))
    (is (= "9,1,22208,6,7,0,99,23,42" (execute-and-get-prg "9,1,22208,6,7,4,99,23,42")))))

(deftest adjust-base-instruction
  (testing "adjust-base positional"
    (is (= "99" (execute-and-get-stdout "00009,5,22204,3,99,1"))))
  (testing "adjust-base immediate"
    (is (= "99" (execute-and-get-stdout "00009,1,22204,3,99,1"))))
  (testing "adjust-base relative"
    (is (= "99" (execute-and-get-stdout "00009,1,22209,0,22204,4,99,1")))))

(deftest halt-instruction
  (is (= [{:ip 0 :halted? false :prg [99]}
          {:ip 0 :halted? true  :prg [99]}]
         (->> (create-computer "99")
              (step-program)
              (map (fn [x] (select-keys x [:ip :prg :halted?])))))))

;; Example programs

(deftest step-program-given-day-two-example-input-then-example-result
  (testing "stepping example program"
    (is (= [{:ip 0 :halted? false :prg [1 9 10 3 2 3 11 0 99 30 40 50]}
            {:ip 4 :halted? false :prg [1 9 10 70 2 3 11 0 99 30 40 50]}
            {:ip 8 :halted? false :prg [3500 9 10 70 2 3 11 0 99 30 40 50]}
            {:ip 8 :halted? true  :prg [3500 9 10 70 2 3 11 0 99 30 40 50]}]
           (->> (create-computer "1,9,10,3,2,3,11,0,99,30,40,50")
                (step-program)
                (map (fn [x] (select-keys x [:ip :prg :halted?]))))))))

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
  (let [final-state (execute-program (create-computer "3,0,4,0,99" "23"))]
    (is (= "23,0,4,0,99" (prg-of final-state)))
    (is (= "23"          (stdout-of final-state)))))

(deftest execute-program-given-day-five-example-input-then-example-stdout
  (let [prg (string/join ["3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,"
                          "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,"
                          "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"])]
    (is (= "999"  (execute-and-get-stdout prg "7")))
    (is (= "1000" (execute-and-get-stdout prg "8")))
    (is (= "1001" (execute-and-get-stdout prg "9")))))

(deftest execute-program-given-day-nine-example-input-then-example-stdout
  (is (= "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" (execute-and-get-stdout "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")))
  (is (= 16 (count (execute-and-get-stdout "1102,34915192,34915192,7,4,7,99,0"))))
  (is (= "1125899906842624" (execute-and-get-stdout "104,1125899906842624,99"))))
