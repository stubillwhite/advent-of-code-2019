(ns advent-of-code-2019.day-12
  (:require [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-12-input.txt"))))

(defn- parse-position [id s]
  (let [regex   #"<x=(.+), y=(.+), z=(.+)>"
        [x y z] (->> (re-seq regex s) (first) (rest) (map parse-long))]
    {:id  id
     :pos [x y z]
     :vel [0 0 0]}))

(defn- parse-input [input]
  (->> input
       (string/split-lines)
       (map-indexed parse-position)))

(defn- velocity-change [a b]
  (cond
    (> a b) -1
    (= a b) 0
    (< a b) +1))

(defn- apply-gravity-to-moon [m moons]
  (reduce (fn [acc x] (assoc acc :vel (mapv + (:vel acc) (mapv velocity-change (:pos acc) (:pos x)))))
          m
          moons))

(defn- apply-gravity [moons]
  (map (fn [m] (apply-gravity-to-moon m (disj moons m)))
       moons))

(defn- apply-velocity [moons]
  (map (fn [m] (assoc m :pos (mapv + (:pos m) (:vel m))))
       moons))

(defn- step-simulation [moons]
  (->> moons
       (apply-gravity)
       (apply-velocity)
       (set)))

(defn simulate [input]
  (iterate step-simulation (into #{} (parse-input input))))

(defn- total-energy-of-moon [{:keys [pos vel]}]
  (let [potential (apply + (map (fn [x] (Math/abs x)) pos))
        kinetic   (apply + (map (fn [x] (Math/abs x)) vel))]
    (* potential kinetic)))

(defn- total-energy-of-system [moons]
  (apply + (map total-energy-of-moon moons)))

(defn solution-part-one [input]
  (-> (simulate input)
      (nth 1000)
      (total-energy-of-system)))

;; Part two

(defn- isolate-plane [plane moon]
  (let [select-plane (fn [x] [(nth x plane)])]
    (-> moon
        (update :pos select-plane)
        (update :vel select-plane))))

(defn find-period [initial-state]
  (loop [state initial-state
         n     0]
    (let [new-state (step-simulation state)]
      (if (= new-state initial-state)
        (inc n)
        (recur new-state (inc n))))))

(defn- find-period-of-plane [moons plane]
  (->> moons
       (map (partial isolate-plane plane))
       (set)
       (find-period)))

(defn- gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))
 
(defn- lcm [a b]
  (/ (* a b) (gcd a b)))

(defn solution-part-two [input]
  (let [moons (parse-input input)]
    (->> [0 1 2]
         (map (partial find-period-of-plane moons))
         (reduce (fn [acc x] (lcm acc x))))))
