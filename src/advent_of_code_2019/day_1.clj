(ns advent-of-code-2019.day-1
  (:require [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-1-input.txt"))))

(defn- parse-input [input]
  (->> input
       (string/split-lines)
       (map #(Long/parseLong %))))

(defn- fuel-required-for [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn solution-part-one [input]
  (->> (parse-input input)
       (map fuel-required-for)
       (reduce +)))

;; Part two

(defn- total-fuel-required-for [mass]
  (let [fuel-required (fuel-required-for mass)]
    (if (<= fuel-required 0)
      0
      (+ fuel-required (total-fuel-required-for fuel-required)))))

(defn solution-part-two [input]
  (->> (parse-input input)
       (map total-fuel-required-for)
       (reduce +)))
