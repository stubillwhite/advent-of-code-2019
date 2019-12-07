(ns advent-of-code-2019.day-6
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-6-input.txt"))))

(defn- parse-input [input]
  (->> (string/split-lines input)
       (map (fn [s] (string/split s #"\)")))))

(defn- build-graph [orbits]
  (reduce (fn [acc [a b]] (assoc acc b a)) {} orbits))

(defn- path-to-centre [graph planet]
  (if (= "COM" planet)
    ["COM"]
    (cons planet (path-to-centre graph (get graph planet)))))

(defn- orbits-of-planet [graph planet]
  (rest (path-to-centre graph planet)))

(defn solution-part-one [input]
  (let [graph (build-graph (parse-input input))]
    (->> (keys graph)
         (map (fn [x] (orbits-of-planet graph x)))
         (map count)
         (apply +))))
