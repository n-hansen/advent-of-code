#!/usr/bin/env bb

(def aoc-year 2021)

(require '[babashka.curl :as curl]
         '[babashka.fs :as fs]
         '[selmer.parser :as selmer])

(def session-id
  (str/trim-newline (slurp ".session_id")))
(def solution-template
  (slurp "templates/Solution.template"))
(def main-template
  (slurp "templates/Main.template"))
(def test-template
  (slurp "templates/Test.template"))


(doseq [day *command-line-args*
        :let [{:keys [status body]}
              (curl/get (format "https://adventofcode.com/%s/day/%s/input" aoc-year day)
                        {:raw-args ["-b" (str "session=" session-id) "--http1.1"]})]]
  (assert (= status 200) (format "Bad response for day %s: %s\n%s" day status body))
  (spit (str "inputs/" day) body)
  (spit (format "src/Puzzles/P%s.hs" day)
        (selmer/render solution-template {:day day}))
  (spit (format "test/P%sSpec.hs" day)
        (selmer/render test-template {:day day})))

(def puzzles
  (->> (fs/glob "src/Puzzles" "P*.hs")
       (keep #(second (re-find #"P([0-9]+)\.hs" (str %))))))

(spit "app/Main.hs"
      (selmer/render main-template {:imports (->> puzzles
                                                  (map #(str "import Puzzles.P" %))
                                                  (str/join \newline))
                                    :vals (->> puzzles
                                               (map #(str "p" %))
                                               (str/join ","))}))
