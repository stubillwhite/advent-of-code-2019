(ns advent-of-code-2019.day-11
  (:require [advent-of-code-2019.computer :as cmp]
            [advent-of-code-2019.computer-io :as cmp-io]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-11-input.txt"))))

(defn- parse-input [input]
  (->> (string/split input #",")
       (map #(Long/parseLong %))
       (into [])))

(defn- create-robot []
  {:dir    :up
   :loc    [0 0]
   :panels {}})

(defn- turn-left [{:keys [dir] :as robot}]
  (assoc robot :dir (dir {:up    :left
                          :left  :down
                          :down  :right
                          :right :up})))

(defn- turn-right [{:keys [dir] :as robot}]
  (assoc robot :dir (dir {:up    :right
                          :right :down
                          :down  :left
                          :left  :up})))

(defn- turn [robot dir]
  (case dir
    0 (turn-left robot)
    1 (turn-right robot)))

(defn- move-forward [{:keys [dir loc] :as robot}]
  (assoc robot :loc
         (let [[x y] loc]
           (case dir
             :up    [x (dec y)]
             :right [(inc x) y]
             :down  [x (inc y)]
             :left  [(dec x) y]))))

(defn- paint [{:keys [loc] :as robot} color]
  (assoc-in robot [:panels loc] color))

(defn- current-panel-color [{:keys [loc] :as robot}]
  (get-in robot [:panels loc] 0))

(defn- paint-and-move [robot color dir]
  (-> robot
      (paint color)
      (turn dir)
      (move-forward)))

(ns-unmap *ns* 'RobotIO)
(defrecord RobotIO [robot color]
  cmp-io/ComputerIO
  (read-from-stdin [this]
    (let [value (current-panel-color (get-in this [:robot]))]
      [value this]))
  
  (write-to-stdout [this v]
    (if-let [color (:color this)]
      (assoc this
             :robot (paint-and-move (:robot this) color v)
             :color nil)
      (assoc this
             :color v))))

(defn solution-part-one [input]
  (let [computer (cmp/initialise-computer (parse-input input) :io (->RobotIO (create-robot) nil))]
    (-> (cmp/execute-program computer)
        (get-in [:io :robot :panels])
        (keys)
        (count))))
