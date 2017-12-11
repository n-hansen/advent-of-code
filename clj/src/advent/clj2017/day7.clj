(ns advent.clj2017.day7
  (:require [advent.clj2017.util :refer :all]
            [blancas.kern.core :as k]
            [blancas.kern.lexer.basic :as l]
            [clojure.set :as set]))

(def input
#_"pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"
  (load "day7-input.txt"))

(def parsed-input
  (let [prog-name (k/<$> (partial apply str) (k/many1 k/alpha-num))
        prog-weight (k/skip-ws (k/>> (k/sym* \( ) (k/<< l/dec-lit (k/sym* \) ))))
        prog-children (k/>> (k/token* " -> ") (k/sep-by1 (k/token* ", ") prog-name))
        prog-listing (k/<*> prog-name prog-weight (k/optional prog-children))
        input-parser (k/sep-by (k/sym* \newline) prog-listing)]
    (k/value input-parser input)))

(def graph
  (->> parsed-input
       (map (fn [[prog _ children]] [prog children]))
       (into {})))

(def part-1
  (->> graph
       vals
       (map set)
       (apply set/union)
       (set/difference (-> graph keys set))
       first))

(def weights
  (->> parsed-input
       (map (fn [[prog weight _]] [prog weight]))
       (into {})))

(def total-weights
  (let [compute-weight (fn cw [node computed]
                         (let [node-weight (weights node)]
                           (cond
                             (nil? (graph node)) [node-weight
                                                 (assoc computed node node-weight)]
                             (computed node) [(computed node) computed]
                             :else (let [children (graph node)
                                         computed* (reduce (fn [cmpt child] (second (cw child cmpt))) computed children)
                                         node-weight* (+ (reduce + (map computed* children)) node-weight)]
                                     [node-weight*
                                      (assoc computed* node node-weight*)]))))]
    (second (compute-weight part-1 {}))))

(def balanced
  (->> (keys graph)
       (filter (fn [node]
                (if (graph node)
                  (apply = (map total-weights (graph node)))
                  true)))
       set))

(def unbalanced
  (set/difference (-> graph keys set) balanced))

(def balanced-children
  (->> (keys graph)
       (filter (fn [node]
                 (if (graph node)
                   (every? balanced (graph node))
                   true)))
       set))

(def problem-parent
  (first (set/intersection balanced-children unbalanced)))

(def part-2
  (let [sibblings (graph problem-parent)
        problem-nodes (->> sibblings
                           (filter #(apply = (map total-weights (remove #{%} sibblings)))))
        problem-node (first problem-nodes)
        correct-total-weight (->> sibblings (remove #{problem-node}) first total-weights)
        delta (- (total-weights problem-node) correct-total-weight)]
    (- (weights problem-node) delta)))
