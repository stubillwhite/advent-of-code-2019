(ns advent-of-code-2019.day-5
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-5-input.txt"))))

(defn- parse-input [input]
  (->> (string/split input #",")
       (map (fn [x] (Long/parseLong x)))))

(defn solution-part-one []
  (->> (cmp/initialise-computer (parse-input problem-input) [1])
       (cmp/execute-program)))
