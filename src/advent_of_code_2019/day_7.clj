(ns advent-of-code-2019.day-7
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.core.async :as async :refer [<!! chan]]
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

(def- series-phase-settings
  (permutations-of-set #{0 1 2 3 4}))

(defn- series-amplifier-stage-output [prg phase input]
  (-> (cmp/initialise-computer prg)
      (cmp/pipe-to-stdin! [phase input])
      (cmp/execute-program)
      (cmp/read-stdout)
      (first)))

(defn series-amplifier-output [input phases]
  (let [prg (parse-input input)]
    (reduce (fn [acc x] (series-amplifier-stage-output prg x acc))
            0
            phases)))

(defn solution-part-one [prg]
  (->> series-phase-settings
       (map (fn [phases] {:phases phases :output (series-amplifier-output prg phases)}))
       (apply max-key :output)))

;; Part two
;;
;; Probably the fastest thing to do is refactor stdin/stdout to use channels

(defn- create-feedback-amplifier [prg]
  (let [a (cmp/initialise-computer prg :id "a")
        b (cmp/initialise-computer prg :id "b" :stdin (:stdout a))
        c (cmp/initialise-computer prg :id "c" :stdin (:stdout b))
        d (cmp/initialise-computer prg :id "d" :stdin (:stdout c))
        e (cmp/initialise-computer prg :id "e" :stdin (:stdout d) :stdout (:stdin a))]
    [a b c d e]))

(defn- execute-feedback-series-amplifier [amps phases]
  (let [[a b c d e] (doall (map (fn [[computer phase]] (cmp/pipe-to-stdin! computer [phase])) (map vector amps phases)))]
    (cmp/pipe-to-stdin! a [0])
    (let [[_ _ _ _ e-halted] (map (fn [x] (async/thread (cmp/execute-program x))) [a b c d e])]
      (<!! e-halted))))

(defn- feedback-series-amplifier-output [input phases]
  (let [amps (create-feedback-amplifier (parse-input input))]
    (->> (execute-feedback-series-amplifier amps phases)
         (cmp/read-stdout)
         (first))))

(def- feedback-series-phase-settings
  (permutations-of-set #{5 6 7 8 9}))

(defn solution-part-two [prg]
  (->> feedback-series-phase-settings
       (map (fn [phases] {:phases phases :output (feedback-series-amplifier-output prg phases)}))
       (apply max-key :output)))
