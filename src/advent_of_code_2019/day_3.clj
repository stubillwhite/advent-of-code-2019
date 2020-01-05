(ns advent-of-code-2019.day-3
  (:require [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-3-input.txt"))))

(defn- parse-step [s]
  (let [[dir & len] s]
    {:dir dir
     :len (parse-long (string/join len))}))

(defn- parse-input [input]
  (->> input
       (string/split-lines)
       (map (fn [x] (map parse-step (string/split x #","))))))

(defn- follow-step [[x y] {:keys [dir len]}]
  (let [deltas (range 1 (inc len))]
    (case dir
      \R (map (fn [dx] [(+ x dx) y]) deltas)
      \L (map (fn [dx] [(- x dx) y]) deltas)
      \U (map (fn [dy] [x (+ y dy)]) deltas)
      \D (map (fn [dy] [x (- y dy)]) deltas))))

(defn- follow-path [path]
  (reduce
   (fn [{:keys [curr points]} dir]
     (let [new-points (follow-step curr dir)]
       {:curr   (last new-points)
        :points (concat points new-points)}))
   {:curr   [0 0]
    :points #{[0 0]}}
   path))

(defn- points-of-intersection [wire-1 wire-2]
  (set/difference
   (set/intersection (into #{} (:points wire-1)) (into #{} (:points wire-2)))
   #{[0 0]}))

(defn- manhattan-distance-from-origin [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn- manhattan-distance-to-intersection [wire-1 wire-2]
  (map manhattan-distance-from-origin (points-of-intersection wire-1 wire-2)))

(defn solution-part-one [input]
  (->> (parse-input input)
       (map follow-path)
       (apply manhattan-distance-to-intersection)
       (apply min)))

;; Part two

(defn- index-of [x coll]
  (first (keep-indexed (fn [idx item] (if (= x item) idx)) coll)))

(defn- total-distance-to-intersection [wire-1 wire-2]
  (for [point (points-of-intersection wire-1 wire-2)]
    (+ (index-of point (:points wire-1))
       (index-of point (:points wire-2)))))

(defn solution-part-two [input]
  (->> (parse-input input)
       (map follow-path)
       (apply total-distance-to-intersection)
       (apply min)))
