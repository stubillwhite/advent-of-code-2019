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

(defn- angles-to-other-asteroids [asteroid asteroids]
  (map (partial angle asteroid) (disj asteroids asteroid)))

(defn visible-asteroids [xy asteroids]
  (count (set (angles-to-other-asteroids xy asteroids))))

(defn solution-part-one [input]
  (let [asteroids (parse-input input)]
    (->> asteroids
         (map (fn [x] {:loc x :visible (visible-asteroids x asteroids)}))
         (apply max-key :visible))))
