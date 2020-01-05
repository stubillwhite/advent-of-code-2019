(ns advent-of-code-2019.day-13
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-13-input.txt"))))

(defn create-game []
  {:grid {}})

(def- object-types
  {0 :empty
   1 :wall
   2 :block
   3 :paddle
   4 :ball})

(defn- object-of-type [id]
  (object-types id))

(defn- add-object [game x y id]
  (assoc-in game [:grid [x y]] (object-of-type id)))

(defn- create-game-state [cmds]
  (->> cmds
       (partition 3)
       (reduce (partial apply add-object) (create-game))))

(defn- count-blocks [{:keys [grid]}]
  (count (for [[k v] grid :when (= v :block)] k)))

(defn- render-object [obj]
  (case obj
    :empty  " "
    :wall   "#"
    :block  "█"
    :paddle "―"
    :ball   "●"))

(defn render-game [{:keys [grid]}]
  (let [width  (->> grid (keys) (map first) (apply max) (inc))
        height (->> grid (keys) (map last)  (apply max) (inc))]
    (->>
     (for [y (range height)
           x (range width)]
       (render-object (get grid [x y] :empty)))
     (partition width)
     (map (partial apply str))
     (string/join "\n"))))

(defn solution-part-one [input]
  (->> (cmp/initialise-computer (cmp/parse-program input))
       (cmp/execute-program)
       (cmp/flush-and-read-stdout)
       (create-game-state)
       (count-blocks)))
