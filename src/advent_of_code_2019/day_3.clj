(ns advent-of-code-2019.day-3
  (:require [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(def- problem-input
  (string/trim (slurp (io/resource "day-3-input.txt"))))

(defn parse-step [s]
  (let [[dir & len] s]
    {:dir dir
     :len (Long/parseLong (string/join len))}))

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
        :points (into points new-points)}))
   {:curr   [0 0]
    :points #{}}
   path))

(defn- manhattan-distance-from-origin [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn solution-part-one [input]
  (->> (parse-input input)
     (map follow-path)
     (map :points)
     (apply set/intersection)
     (map manhattan-distance-from-origin)
     (apply min)))
