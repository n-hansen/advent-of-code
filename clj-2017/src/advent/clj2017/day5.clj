(ns advent.clj2017.day5
  (:require [advent.clj2017.util :refer :all]
            [clojure.string :as s]))

(def input
  #_"0 3 0 1 -3"
  (load "day5-input.txt"))

(def part-1
  (loop [instructions (as-> input i
                        (s/split i #"\s+")
                        (mapv read-string i))
         pointer 0
         steps 0]
    (if (>= pointer (count instructions))
      steps
      (recur
       (update instructions pointer inc)
       (+ pointer (instructions pointer))
       (inc steps)))))

(def part-2
  (loop [instructions (as-> input i
                        (s/split i #"\s+")
                        (mapv read-string i))
         pointer 0
         steps 0]
    (if (or (>= pointer (count instructions))
            (< pointer 0))
      steps
      (recur
       (update instructions pointer (if (>= (instructions pointer) 3)
                                      dec
                                      inc))
       (+ pointer (instructions pointer))
       (inc steps)))))
