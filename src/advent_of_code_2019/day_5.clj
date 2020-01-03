(ns advent-of-code-2019.day-5
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.computer-io :as cmp-io]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-5-input.txt"))))

(defn- parse-input [input]
  (->> (string/split input #",")
       (map (fn [x] (Long/parseLong x)))))

(defn solution-part-one []
  (-> (cmp/initialise-computer (parse-input problem-input))
      (cmp/buffer-to-stdin [1])
      (cmp/execute-program)
      (cmp/flush-and-read-stdout)))



;; Part two

(defn solution-part-two []
  (-> (cmp/initialise-computer (parse-input problem-input))
      (cmp/buffer-to-stdin [5])
      (cmp/execute-program)
      (cmp/flush-and-read-stdout)))

