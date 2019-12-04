(ns advent-of-code-2019.day-4
  (:require [advent-of-code-2019.utils :refer [def-]]))

(def- lower-bound 172851)
(def- upper-bound 675869)

(defn digits-increase? [s]
  (= (seq s) (sort s)))

(defn two-adjacent-characters? [s]
  (re-find #"(.)\1" s))

(def valid-basic-password?
  (every-pred digits-increase? two-adjacent-characters?))

(defn solution-part-one []
  (->> (for [x (range lower-bound (inc upper-bound))] (str x))
       (filter valid-basic-password?)
       (count)))

;; Part two

(defn exactly-two-adjacent-characters? [s]
  (->> (partition-by identity s)
       (some (fn [x] (= (count x) 2)))))

(def valid-strict-password?
  (every-pred digits-increase? two-adjacent-characters? exactly-two-adjacent-characters?))

(defn solution-part-two []
  (->> (for [x (range lower-bound (inc upper-bound))] (str x))
       (filter valid-strict-password?)
       (count)))

