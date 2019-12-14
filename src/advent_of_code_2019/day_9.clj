(ns advent-of-code-2019.day-9
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-9-input.txt"))))

(defn- parse-input [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn solution-part-one [input]
  (-> (parse-input input)
      (cmp/initialise-computer)
      (cmp/pipe-to-stdin! [1])
      (cmp/execute-program)
      (cmp/read-stdout)))

;; Part two

(defn solution-part-two [input]
  (-> (parse-input input)
      (cmp/initialise-computer)
      (cmp/pipe-to-stdin! [2])
      (cmp/execute-program)
      (cmp/read-stdout)))

