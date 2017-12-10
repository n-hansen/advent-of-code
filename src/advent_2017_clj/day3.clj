(ns advent-2017-clj.day3
  (:require [clojure.math.numeric-tower :as m]))

(def input
  368078)

(def ring (-> input
              m/exact-integer-sqrt
              ((fn [[x r]]
                 (if (= r 0) x (inc x))))
              ((fn [x]
                 (if (odd? x) (/ (dec x) 2) (/ x 2))))))

(def side-len (inc (* 2 ring)))

(def part-1
  (as-> input x
    (- x (m/expt (- side-len 2) 2))
    (iterate #(- % (dec side-len)) x)
    (drop-while #(> % (dec side-len)) x)
    (first x)
    (- x ring)
    (m/abs x)
    (+ x ring)))

(def part-2
  (->> [0 0 :r]
       (iterate (fn [[x y direction]]
                  (case direction
                    :r [(inc x) y
                        (if (= x (- y)) :u :r)]
                    :u [x (inc y)
                        (if (= x (inc y)) :l :u)]
                    :l [(dec x) y
                        (if (= (- x) (dec y)) :d :l)]
                    :d [x (dec y)
                        (if (= x (dec y)) :r :d)])))
       (map pop)
       rest
       (#(loop [[[x y :as loc] & rest] %
                seen {[0 0] 1}]
           (let [neighbors [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                            [(dec x)      y ]   #_[x y]   [(inc x)      y ]
                            [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]
                 value (->> neighbors
                            (map seen)
                            (remove nil?)
                            (reduce +))]
             (if (> value input)
               value
               (recur rest (assoc seen loc value))))))))
