(ns advent-of-code-2019.day-6
  (:require [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.set :as set]
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

;; Part two

(defn- path-to-each-other [graph a b]
  (let [a-xfers (path-to-centre graph a)
        b-xfers (path-to-centre graph b)]
    (set/difference
     (set/union 
      (set/difference (set a-xfers) (set b-xfers))
      (set/difference (set b-xfers) (set a-xfers)))
     #{a b})))

(defn solution-part-two [input]
  (let [graph (build-graph (parse-input input))]
    (count (path-to-each-other graph "YOU" "SAN"))))
