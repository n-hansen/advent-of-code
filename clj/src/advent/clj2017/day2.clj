(ns advent.clj2017.day2
  (:require [advent.clj2017.util :refer :all]
            [blancas.kern.core :as k]
            [blancas.kern.lexer.basic :as l]))

(def input (load "day2-input.txt"))

(def parsed-input
  (let [entry k/dec-num
        row (k/sep-by (k/sym* \tab) entry)
        sheet (k/sep-by (k/sym* \newline) row)]
    (k/value sheet input)))

(def checksum
  (->> parsed-input
       (map #(- (apply max %) (apply min %)))
       (reduce +)))

(defn divisor?
  [a b]
  (= 0 (mod (max a b) (min a b))))

(defn process-row
  [[x & xs]]
  (if-let [y (first (filter #(divisor? x %) xs))]
    (/ (max x y) (min x y))
    (process-row xs)))

(def part-2
  (->> parsed-input
       (map process-row)
       (reduce +)))
