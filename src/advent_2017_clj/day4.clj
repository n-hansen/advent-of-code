(ns advent-2017-clj.day4
  (:require [advent-2017-clj.util :refer :all]
            [clojure.string :as s]))

(def input (load "day4-input.txt"))

(def part-1
  (->> input
       s/split-lines
       (map #(s/split % #" "))
       (filter #(= (count %) (count (set %))))
       count))

(def part-2
  (->> input
       s/split-lines
       (map #(map sort (s/split % #" ")))
       (filter #(= (count %) (count (set %))))
       count))
