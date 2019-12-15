(ns advent-of-code-2019.day-10-test
  (:require [advent-of-code-2019.day-10 :refer :all]
            [advent-of-code-2019.utils :refer [def-]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(defn- to-string [width height m]
  (->> (for [y (range height)
             x (range width)] (get m [x y] "."))
       (partition width)
       (map string/join)
       (string/join "\n")))

(defn- to-input [strs]
  (string/join "\n" strs))

(defn- to-output [strs]
  (string/join "\n" strs))

(def- example-1-input (to-input [".#..#"
                                 "....."
                                 "#####"
                                 "....#"
                                 "...##"]))

(def- example-1-output (to-output [".7..7"
                                   "....."
                                   "67775"
                                   "....7"
                                   "...87"]))

(def- example-2-input (to-input ["......#.#."
                                 "#..#.#...."
                                 "..#######."
                                 ".#.#.###.."
                                 ".#..#....."
                                 "..#....#.#"
                                 "#..#....#."
                                 ".##.#..###"
                                 "##...#..#."
                                 ".#....####"]))

(def- example-3-input (to-input ["#.#...#.#."
                                 ".###....#."
                                 ".#....#..."
                                 "##.#.#.#.#"
                                 "....#.#.#."
                                 ".##..###.#"
                                 "..#...##.."
                                 "..##....##"
                                 "......#..."
                                 ".####.###."]))

(def- example-4-input (to-input [".#..#..###"
                                 "####.###.#"
                                 "....###.#."
                                 "..###.##.#"
                                 "##.##.#.#."
                                 "....###..#"
                                 "..#.#..#.#"
                                 "#..#.#.###"
                                 ".##...##.#"
                                 ".....#.#.."]))

(def- example-5-input (to-input [".#..##.###...#######"
                                 "##.############..##."
                                 ".#.######.########.#"
                                 ".###.#######.####.#."
                                 "#####.##.#.##.###.##"
                                 "..#####..#.#########"
                                 "####################"
                                 "#.####....###.#.#.##"
                                 "##.#################"
                                 "#####.##.###..####.."
                                 "..######..##.#######"
                                 "####.##.####...##..#"
                                 ".#####..#.######.###"
                                 "##...#.##########..."
                                 "#.##########.#######"
                                 ".####.#.###.###.#.##"
                                 "....##.##.###..#####"
                                 ".#.#.###########.###"
                                 "#.#.#.#####.####.###"
                                 "###.##.####.##.#..##"]))

(defn- map-visible-asteroids [input width height]
  (let [asteroids (parse-input input)]
    (->> asteroids
         (map (fn [x] [x (visible-asteroids x asteroids)]))
         (into {})
         (to-string width height))))

(deftest visible-asteroids-given-example-input-then-example-result
  (testing "example one"
    (is (= example-1-output (map-visible-asteroids example-1-input 5 5)))))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= {:loc [3  4]  :visible 8}   (solution-part-one example-1-input)))
  (is (= {:loc [5  8]  :visible 33}  (solution-part-one example-2-input)))
  (is (= {:loc [1  2]  :visible 35}  (solution-part-one example-3-input)))
  (is (= {:loc [6  3]  :visible 41}  (solution-part-one example-4-input)))
  (is (= {:loc [11 13] :visible 210} (solution-part-one example-5-input))))
