(ns advent-of-code-2019.day-12
  (:require [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-12-input.txt"))))

(defn- parse-position [s]
  (let [regex   #"<x=(.+), y=(.+), z=(.+)>"
        [x y z] (->> (re-seq regex s) (first) (rest) (map parse-long))]
    {:pos [x y z]
     :vel [0 0 0]}))

(defn- parse-input [input]
  (->> input
       (string/split-lines)
       (map parse-position)))

(def- example-input
  (string/join "\n"
               ["<x=-1, y=0, z=2>"
                "<x=2, y=-10, z=-7>"
                "<x=4, y=-8, z=8>"
                "<x=3, y=5, z=-1>"]))

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
       (into #{})))

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


