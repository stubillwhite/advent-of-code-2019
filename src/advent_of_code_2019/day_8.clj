(ns advent-of-code-2019.day-8
  (:require [advent-of-code-2019.utils :refer [def-]]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(def- problem-input
  (string/trim (slurp (io/resource "day-8-input.txt"))))

(defn- parse-input [input]
  (->> input
       (map #(Long/parseLong (str %)))))

(defn- to-layers [width height pixels]
  (->> pixels
       (partition width)
       (partition height)
       (map (fn [xs] (apply concat xs)))))

(defn- count-of-value [v layer]
  (get (frequencies layer) v 0))

(defn- fewest-of-value [v layers]
  (reduce
   (fn [acc x] (if (< (count-of-value v x) (count-of-value v acc)) x acc))
   layers))

(defn- integrity-check [layer]
  (let [freqs (frequencies layer)]
    (* (get freqs 1) (get freqs 2))))

(defn solution-part-one [input width height]
  (->> (parse-input input)
       (to-layers width height)
       (fewest-of-value 0)
       (integrity-check)))

;; Part two

(defn- transparent? [v]
  (= v 2))

(defn- to-visible-pixels [layers]
  (->> layers
       (apply (partial map vector))
       (map (partial drop-while transparent?))
       (map first)))

(defn- render-pixels [width pixels]
  (->> pixels
       (partition width)
       (map string/join)
       (string/join "\n")))

(defn solution-part-two [input width height]
  (->> (parse-input input)
       (to-layers width height)
       (to-visible-pixels)
       (render-pixels width)))

(defn display-result [s]
  (-> s
      (string/replace #"0" " ")
      (string/replace #"1" "#")
      (format "\n%s\n")
      (println)))
