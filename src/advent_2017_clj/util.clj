(ns advent-2017-clj.util
  (:require [clojure.java.io :as io]))

(defn load
  [r]
  (-> r
      io/resource
      io/file
      slurp
      .trim))
