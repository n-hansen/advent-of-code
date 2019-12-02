(ns advent.clj2017.util
  (:require [clojure.java.io :as io]))

(defn load
  [r]
  (-> r
      io/resource
      io/file
      slurp
      .trim))
