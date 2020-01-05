(ns advent-of-code-2019.day-13
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.computer-io :as cmp-io]
            [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def problem-input
  (string/trim (slurp (io/resource "day-13-input.txt"))))

(defn- create-game []
  {:grid   {}
   :paddle nil
   :ball   nil
   :score  0})

(def- object-types
  {0 :empty
   1 :wall
   2 :block
   3 :paddle
   4 :ball})

(defn- object-type [x y id]
  (if (= [x y] [-1 0])
    :score
    (object-types id)))

(defn- update-attributes [game x y id]
  (case (object-type x y id)
    :score  (assoc game :score  id)
    :paddle (assoc game :paddle [x y])
    :ball   (assoc game :ball   [x y])
    game))

(defn- update-grid [game x y id]
  (let [obj (object-type x y id)]
    (case obj
      :score game
      (assoc-in game [:grid [x y]] obj))))

(defn- update-state [game [x y id]]
  (-> game
      (update-attributes x y id)
      (update-grid x y id)))

(defn- create-game-state [cmds]
  (->> cmds
       (partition 3)
       (reduce update-state (create-game))))

(defn- count-blocks [{:keys [grid]}]
  (count (for [[k v] grid :when (= v :block)] k)))

(defn solution-part-one [input]
  (->> (cmp/initialise-computer (cmp/parse-program input))
       (cmp/execute-program)
       (cmp/flush-and-read-stdout)
       (create-game-state)
       (count-blocks)))

;; Part two

(defn- render-object [obj]
  (case obj
    :empty  " "
    :wall   "|"
    :block  "#"
    :paddle "="
    :ball   "o"))

(defn- render-grid [{:keys [grid]}]
  (let [width  (->> grid (keys) (map first) (apply max) (inc))
        height (->> grid (keys) (map last)  (apply max) (inc))]
    (->>
     (for [y (range height)
           x (range width)]
       (render-object (get grid [x y] :empty)))
     (partition width)
     (map (partial apply str))
     (string/join "\n"))))

(defn- render-game [{:keys [grid score] :as game}]
  (let [height (->> grid (keys) (map last)  (apply max) (inc))]
    (str (format "\nScore: %d\n" score)
         (render-grid game)
         (format "\033[%dA" (+ height 3)))))

(ns-unmap *ns* 'GameIO)
(defrecord GameIO [game cmd]
  cmp-io/ComputerIO
  (read-from-stdin [this]
    (let [[bx _] (get-in this [:game :ball])
          [px _] (get-in this [:game :paddle])
          value  (cond
                   (> bx px)  1
                   (= bx px)  0
                   (< bx px) -1)]
      [value this]))
  
  (write-to-stdout [this v]
    (let [cmd (conj (get-in this [:game :cmd]) v)]
      (if (= 3 (count cmd))
        (-> this
            (update-in [:game] update-state cmd)
            ;; ((fn [x] (println (render-game (:game x))) x))
            (assoc-in  [:game :cmd] []))
        (-> this
            (update-in [:game :cmd] (fn [x] (conj x v))))))))

(defn- game-io []
  (->GameIO (create-game) []))

(defn solution-part-two [input]
  (let [prg (assoc (cmp/parse-program input) 0 2)]
    (-> (cmp/initialise-computer prg :io (game-io))
        (cmp/execute-program)
        (get-in [:io :game :score]))))


