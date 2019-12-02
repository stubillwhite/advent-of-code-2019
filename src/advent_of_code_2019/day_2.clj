(ns advent-of-code-2019.day-2
  (:require [advent-of-code-2019.utils :refer [def- defmethod- defmulti-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-2-input.txt"))))

(defn- reset-program [prg]
  (assoc prg
         1 12
         2  2))

(defn parse-input [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn- initialise-computer [prg]
  {:pc      0
   :prg     prg
   :halted? false})

(defmulti- execute-instruction (fn [{:keys [pc prg]}] (nth prg pc)))

(defmethod- execute-instruction 1 [{:keys [pc prg] :as computer}]
  (let [[_ a b output] (drop pc prg)]
    (assoc computer
           :pc  (+ pc 4)
           :prg (assoc prg output (+ (nth prg a) (nth prg b))))))

(defmethod- execute-instruction 2 [{:keys [pc prg] :as computer}]
  (let [[_ a b output] (drop pc prg)]
    (assoc computer
           :pc  (+ pc 4)
           :prg (assoc prg output (* (nth prg a) (nth prg b))))))

(defmethod- execute-instruction 99 [{:keys [pc] :as computer}]
  (assoc computer
         :pc      (+ pc 1)
         :halted? true))

(defn step-program [prg]
  (take-while (fn [{:keys [halted?]}] (not halted?))
              (iterate execute-instruction (initialise-computer prg))))

(defn solution-part-one [input]
  (->> (parse-input input)
       (reset-program)
       (step-program)
       (last)
       (:prg)
       (first)))

