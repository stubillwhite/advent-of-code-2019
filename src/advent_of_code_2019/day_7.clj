(ns advent-of-code-2019.day-7
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-7-input.txt"))))

(defn- parse-input [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn- permutations-of-set [items]
  (if (= (count items) 1)
    [items]
    (mapcat (fn [x] (map (partial cons x) (permutations-of-set (disj items x))))
            items)))

(def- phase-settings
  (permutations-of-set #{0 1 2 3 4}))

(defn- amplifier-output [prg phase input]
  (->> (cmp/initialise-computer prg [phase input])
       (cmp/execute-program)
       (:stdout)
       (first)))

(defn amplifier-series-output [input phases]
  (let [prg (parse-input input)]
    (reduce (fn [acc x] (amplifier-output prg x acc))
            0
            phases)))

(defn solution-part-one [prg]
  (->> phase-settings
       (map (fn [phases] {:phases phases :output (amplifier-series-output prg phases)}))
       (apply max-key :output)))
