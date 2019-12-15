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

(def- example-6-input (to-input [".#....#####...#.."
                                 "##...##.#####..##"
                                 "##...#...#.#####."
                                 "..#.....#...###.."
                                 "..#.#.....#....##"]))

(defn- map-visible-asteroids [input width height]
  (let [asteroids (parse-input input)]
    (->> asteroids
         (map (fn [x] [x (asteroids-visible-from x asteroids)]))
         (into {})
         (to-string width height))))

(deftest visible-asteroids-given-example-input-then-example-result
  (testing "example one"
    (is (= example-1-output (map-visible-asteroids example-1-input 5 5)))))

(deftest solution-part-one-given-example-input-then-example-result
  (is (= {:location [ 3  4] :visible 8}   (solution-part-one example-1-input)))
  (is (= {:location [ 5  8] :visible 33}  (solution-part-one example-2-input)))
  (is (= {:location [ 1  2] :visible 35}  (solution-part-one example-3-input)))
  (is (= {:location [ 6  3] :visible 41}  (solution-part-one example-4-input)))
  (is (= {:location [11 13] :visible 210} (solution-part-one example-5-input))))

(defn- get-vaporization-sequence [input point]
  (map :location (vaporization-sequence input point)))

(deftest vaporization-sequence-given-example-input-then-expected-sequence
  (is (= [[3 2] [4 0] [4 2] [4 3] [4 4] [0 2] [1 2] [2 2] [1 0]]
         (get-vaporization-sequence example-1-input [3 4])))
  (is (= [[ 8 1] [ 9 0] [ 9 1] [10 0] [ 9 2] [11 1] [12 1] [11 2] [15 1]
          [12 2] [13 2] [14 2] [15 2] [12 3] [16 4] [15 4] [10 4] [ 4 4]
          [ 2 4] [ 2 3] [ 0 2] [ 1 2] [ 0 1] [ 1 1] [ 5 2] [ 1 0] [ 5 1]
          [ 6 1] [ 6 0] [ 7 0] [ 8 0] [10 1] [14 0] [16 1] [13 3] [14 3]]
         (get-vaporization-sequence example-6-input [8 3]))))

(deftest vaporization-sequence-give-example-input-then-example-output
  (let [sequence (get-vaporization-sequence example-5-input [11 13])]
    (is (= [11 12] (nth sequence 0)))
    (is (= [12  1] (nth sequence 1)))
    (is (= [12  2] (nth sequence 2)))
    (is (= [12  8] (nth sequence 9)))
    (is (= [16  0] (nth sequence 19)))
    (is (= [16  9] (nth sequence 49)))
    (is (= [10 16] (nth sequence 99)))
    (is (= [ 9  6] (nth sequence 198)))
    (is (= [ 8  2] (nth sequence 199)))
    (is (= [10  9] (nth sequence 200)))
    (is (= [11  1] (nth sequence 298)))))
