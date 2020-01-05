(ns advent-of-code-2019.day-2
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-2-input.txt"))))

(defn- initialise-program [{:keys [prg] :as cmp} noun verb]
  (assoc cmp :prg
         (assoc prg
                1 noun
                2 verb)))

(defn solution-part-one []
  (-> (cmp/initialise-computer (cmp/parse-program problem-input))
      (initialise-program 12 2)
      (cmp/execute-program)
      (:prg)
      (first)))

;; Part two

(def- nouns-and-verbs
  (for [n (range 100)
        v (range 100)]
    [n v]))

(defn solution-part-two []
  (let [computer   (cmp/initialise-computer (cmp/parse-program problem-input))
        calc-input (fn [[n v]] (+ (* 100 n) v))]
    (->> nouns-and-verbs
         (filter (fn [[n v]] (= 19690720 (->> (initialise-program computer n v)
                                             (cmp/execute-program)
                                             (:prg)
                                             (first)))))
         (first)
         (calc-input))))
