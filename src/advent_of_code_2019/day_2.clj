(ns advent-of-code-2019.day-2
  (:require [advent-of-code-2019.utils :refer [def- defmethod- defmulti-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-2-input.txt"))))

(defn- reset-program [prg noun verb]
  (assoc prg
         1 noun
         2 verb))

(defn parse-input [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn- initialise-computer [prg]
  {:ip      0
   :prg     prg
   :halted? false})

(defmulti- execute-instruction (fn [{:keys [ip prg]}] (nth prg ip)))

(defmethod- execute-instruction 1 [{:keys [ip prg] :as computer}]
  (let [[_ a b output] (drop ip prg)]
    (assoc computer
           :ip  (+ ip 4)
           :prg (assoc prg output (+ (nth prg a) (nth prg b))))))

(defmethod- execute-instruction 2 [{:keys [ip prg] :as computer}]
  (let [[_ a b output] (drop ip prg)]
    (assoc computer
           :ip  (+ ip 4)
           :prg (assoc prg output (* (nth prg a) (nth prg b))))))

(defmethod- execute-instruction 99 [{:keys [ip] :as computer}]
  (assoc computer
         :ip      (+ ip 1)
         :halted? true))

(defn step-program [prg]
  (take-while (fn [{:keys [halted?]}] (not halted?))
              (iterate execute-instruction (initialise-computer prg))))

(defn- execute-program [prg n v]
  (-> (reset-program prg n v)
      (step-program)
      (last)
      (:prg)
      (first)))

(defn solution-part-one [input]
  (execute-program (parse-input input) 12 2))

;; Part two

(def- nouns-and-verbs
  (for [n (range 100)
        v (range 100)]
    [n v]))

(defn solution-part-two [input]
  (let [prg        (parse-input input)
        calc-input (fn [[n v]] (+ (* 100 n) v))]
    (->> nouns-and-verbs
         (filter (fn [[n v]] (= (execute-program prg n v) 19690720)))
         (first)
         (calc-input))))
