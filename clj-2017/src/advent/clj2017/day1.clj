(ns advent.clj2017.day1
  (:require [clojure.java.io :as io]))

(def input
  (-> "day1-input.txt"
      io/resource
      io/file
      slurp
      .trim))

(def shifted-input
  (apply str (concat (rest input) [(first input)])))

(def part-1
  (->> (map #(vector %1 %2) input shifted-input)
       (filter (partial apply =))
       (map (comp read-string str first))
       (reduce +)))

(def big-shifted-input
  (let [shift (/ (count input) 2)]
    (apply str (concat (drop shift input) (take shift input)))))

(def part-2
  (->> (map #(vector %1 %2) input big-shifted-input)
       (filter (partial apply =))
       (map (comp read-string str first))
       (reduce +)))
