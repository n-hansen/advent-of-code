#!/usr/bin/env bb

(def aoc-year 2021)

(require '[babashka.curl :as curl]
         '[babashka.fs :as fs])

(def session-id
  (slurp ".session_id"))

(def puzzle-template
  (slurp "src/Puzzles/Template.hs"))

(def main-template
  (slurp "app/Template.hs"))

(doseq [day *command-line-args*
        :let [{:keys [status body]}
              (curl/get (format "https://adventofcode.com/%s/day/%s/input" aoc-year day)
                        {:raw-args ["-b" (str "session=" session-id) "--http1.1"]})]]
  (assert (= status 200) (format "Bad response for day %s: %s\n%s" day status body))
  (spit (str "inputs/" day) body)
  (spit (format "src/Puzzles/P%s.hs" day)
        (-> puzzle-template
            (str/replace #"module Puzzles\.Template" (str "module Puzzles.P" day))
            (str/replace #"PUZZLE" (str day)))))

(def puzzles
  (->> (fs/glob "src/Puzzles" "P*.hs")
       (keep #(second (re-find #"P([0-9]+)\.hs" (str %))))))

(spit "app/Main.hs"
      (-> main-template
          (str/replace #"\{-PUZZLE_IMPORTS-\}"
                       (->> puzzles
                            (map #(str "import Puzzles.P" %))
                            (str/join \newline)))
          (str/replace #"\{-PUZZLE_FNS-\}"
                       (->> puzzles
                            (map #(str "p" %))
                            (str/join ",")))
          (str/replace #"module Template" "module Main")))
