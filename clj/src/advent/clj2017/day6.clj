(ns advent.clj2017.day6
  (:require [advent.clj2017.util :refer :all]
            [clojure.string :as s]))

(def input
  #_"0 2 7 0"
  (load "day6-input.txt"))

(defn maximum
  [coll]
  (loop [[x & xs] coll
         m (first coll)]
    (if (empty? xs)
      (if (> (compare x m) 0) x m)
      (if (> (compare x m) 0)
        (recur xs x)
        (recur xs m)))))

(def part-1
  (loop [registers (as-> input i
                     (s/split i #"\s+")
                     (mapv read-string i))
         seen #{registers}
         steps 0]
    (let [[blocks i*] (->> registers
                          (map-indexed #(vector %2 (- %1)))
                          maximum)
          i (- i*)
          increment (quot blocks (count registers))
          remainder (->> (range (inc i) (+ i (rem blocks (count registers)) 1))
                         (map #(mod % (count registers)))
                         set)
          registers* (as-> registers r
                       (assoc r i 0)
                       (map-indexed (fn [j v]
                                      (if (remainder j)
                                        (+ v increment 1)
                                        (+ v increment)))
                                    r)
                       (vec r))]
      (if (seen registers*)
        (inc steps)
        (recur registers* (conj seen registers*) (inc steps))))))

(def part-2
  (loop [registers (as-> input i
                     (s/split i #"\s+")
                     (mapv read-string i))
         seen {registers 0}
         steps 0]
    (let [[blocks i*] (->> registers
                          (map-indexed #(vector %2 (- %1)))
                          maximum)
          i (- i*)
          increment (quot blocks (count registers))
          remainder (->> (range (inc i) (+ i (rem blocks (count registers)) 1))
                         (map #(mod % (count registers)))
                         set)
          registers* (as-> registers r
                       (assoc r i 0)
                       (map-indexed (fn [j v]
                                      (if (remainder j)
                                        (+ v increment 1)
                                        (+ v increment)))
                                    r)
                       (vec r))]
      (if (seen registers*)
        (inc (- steps (seen registers*)))
        (recur registers* (conj seen [registers* (inc steps)]) (inc steps))))))
