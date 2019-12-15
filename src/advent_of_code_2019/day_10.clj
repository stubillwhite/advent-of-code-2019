(ns advent-of-code-2019.day-10
  (:require [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-10-input.txt"))))

(defn parse-input [input]
  (->> input
       (string/split-lines)
       (map-indexed (fn [y chs] (map-indexed (fn [x ch] (when (= \# ch) [x y])) chs)))
       (apply concat)
       (filter identity)
       (set)))

(defn- angle [[x1 y1] [x2 y2]]
  (Math/atan2 (- y2 y1) (- x2 x1)))

(defn- distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn- asteroid-bearings-from [point asteroids]
  (map
   (fn [location]
     {:location location
      :angle    (angle point location)
      :distance (distance point location)})
   (disj asteroids point)))

(defn asteroids-visible-from [point asteroids]
  (count (set (map :angle (asteroid-bearings-from point asteroids)))))

(defn solution-part-one [input]
  (let [asteroids (parse-input input)]
    (->> asteroids
         (map (fn [x] {:location x :visible (asteroids-visible-from x asteroids)}))
         (apply max-key :visible))))

;; Part two

(defn- group-by-angle [infos]
  (reduce
   (fn [acc info] (update acc (:angle info) (partial cons info)))
   {}
   infos))

(defn- group-by-angle-and-order-by-distance [infos]
  (->> infos
       (group-by-angle)
       (map (fn [[angle infos]] [angle (sort-by :distance infos)]))
       (sort-by (fn [[angle infos]] [angle infos]))))

(defn- vaporize-and-move-on [data]
  (if (empty? data)
    nil
    (let [[angle planets] (first data)]
      (cons (first planets)
            (lazy-seq
             (vaporize-and-move-on
              (if (empty? (rest planets))
                (rest data)
                (concat (rest data) [[angle (rest planets)]]))))))))

(defn- rotate-while [f coll]
  (take (count coll) (drop-while f (cycle coll))))

(defn- starting-directly-up [locations]
  (rotate-while (fn [[angle info]] (< angle (/ Math/PI -2))) locations))

(defn vaporization-sequence [input laser-location]
  (->> (parse-input input)
       (asteroid-bearings-from laser-location)
       (group-by-angle-and-order-by-distance)
       (starting-directly-up)
       (vaporize-and-move-on)))

(defn solution-part-two [input]
  (let [laser-location     (:location (solution-part-one input))
        {:keys [location]} (nth (vaporization-sequence input laser-location) 199)
        [x y]              location]
    (+ (* x 100) y)))
