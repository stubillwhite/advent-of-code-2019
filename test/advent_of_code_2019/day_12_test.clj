(ns advent-of-code-2019.day-12-test
  (:require [advent-of-code-2019.day-12 :refer :all]
            [advent-of-code-2019.utils :refer [def- parse-long]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(defn- make-input [& lines]
  (string/join "\n" (sort lines)))

(def- example-input-one
  (string/join "\n"
               ["<x=-1, y=0, z=2>"
                "<x=2, y=-10, z=-7>"
                "<x=4, y=-8, z=8>"
                "<x=3, y=5, z=-1>"]))

(def- example-input-two
  (string/join "\n"
               ["<x=-8, y=-10, z=0>"
                "<x=5, y=5, z=10>"
                "<x=2, y=-7, z=3>"
                "<x=9, y=-8, z=-3>"]))

(defn- parse-example-result-line [id s]
  (let [regex #"pos=<x=([^,]+), y=([^,]+), z=([^,]+)>, vel=<x=([^,]+), y=([^,]+), z=([^,]+)>"
        [a b c d e f] (->> (re-seq regex s) (first) (rest) (map parse-long))]
    {:id  id
     :pos [a b c]
     :vel [d e f]}))

(defn- parse-example-result [& lines]
  (->> lines
       (map-indexed parse-example-result-line)
       (into #{})))

(def- after-0-steps
  (parse-example-result
   "pos=<x=-1, y=  0, z= 2>, vel=<x= 0, y= 0, z= 0>"
   "pos=<x= 2, y=-10, z=-7>, vel=<x= 0, y= 0, z= 0>"
   "pos=<x= 4, y= -8, z= 8>, vel=<x= 0, y= 0, z= 0>"
   "pos=<x= 3, y=  5, z=-1>, vel=<x= 0, y= 0, z= 0>"))

(def- after-1-steps
  (parse-example-result
   "pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>"
   "pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>"
   "pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>"
   "pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>"))

(def- after-5-steps
  (parse-example-result
   "pos=<x=-1, y=-9, z= 2>, vel=<x=-3, y=-1, z= 2>"
   "pos=<x= 4, y= 1, z= 5>, vel=<x= 2, y= 0, z=-2>"
   "pos=<x= 2, y= 2, z=-4>, vel=<x= 0, y=-1, z= 2>"
   "pos=<x= 3, y=-7, z=-1>, vel=<x= 1, y= 2, z=-2>"))

(def- after-10-steps
  (parse-example-result
   "pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>"
   "pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>"
   "pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>"
   "pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>"))

(defn- simulation-step [simulation n]
  (into #{} (nth simulation n)))

(deftest simulate-given-example-input-then-example-result
  (let [simulation (simulate example-input-one)]
    (is (= after-0-steps  (simulation-step simulation 0)))
    (is (= after-1-steps  (simulation-step simulation 1)))
    (is (= after-5-steps  (simulation-step simulation 5)))
    (is (= after-10-steps (simulation-step simulation 10)))))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= 183 (solution-part-one example-input-one))))

(deftest solution-part-two-given-example-input-then-example-result
  (is (= 2772       (solution-part-two example-input-one)))
  (is (= 4686774924 (solution-part-two example-input-two))))
