#!/usr/bin/env bb

(deps/add-deps '{:deps {org.clojure/data.zip {:mvn/version "1.0.0"}}})
(require '[babashka.fs :as fs]
         '[babashka.curl :as curl]
         '[clojure.data.zip.xml :as zxml]
         '[clojure.data.xml :as xml]
         '[clojure.zip :as z])

(def aoc-year 2021)

(def cli-opts
  (tools.cli/parse-opts *command-line-args*
                        [["-1" "--pt1" nil
                          :id :pt1]
                         ["-2" "--pt2" nil
                          :id :pt2]]))

(def session-id
  (str/trim-newline (slurp ".session_id")))

(def day
  (->> (fs/glob "src/Puzzles" "P*.hs")
       (keep #(second (re-find #"P([0-9]+)\.hs" (str %))))
       (apply max-key #(Integer/parseInt %))))

(def result
  (let [{:keys [out exit err]} (shell/sh "stack" "run" day)
        _ (assert (zero? exit) (str "Non-zero exit. Error stream:\n" err))
        [_ pt1] (re-find #"(?m)(?: -- Part 1 --\n(.*)\n)" out)
        [_ pt2] (re-find #"(?m)(?: -- Part 2 --\n(.*)\n)" out)]
    (cond
      (and (-> cli-opts :options :pt1) pt1) {:answer pt1 :part "1"}
      (and (-> cli-opts :options :pt2) pt2) {:answer pt2 :part "2"}
      pt2 {:answer pt2 :part "2"}
      pt1 {:answer pt1 :part "1"}
      :else (assert false "Nothing to submit!"))))

(println (format "Submitting answer to day %s part %s: %s" day (:part result) (:answer result)))

(-> (curl/post (format "https://adventofcode.com/%s/day/%s/answer" aoc-year day)
               {:body (format "level=%s&answer=%s" (:part result) (:answer result))
                :headers {"Cookie" (str "session=" session-id)
                          "Content-Type" "application/x-www-form-urlencoded"}
                :raw-args ["--http1.1"]})
    :body
    ;; the java XML parser is finicky and needs some handholding to parse the response
    (str/replace #"<link [^>]+/>" "")
    (str/replace "<!DOCTYPE html>" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
    xml/parse-str
    z/xml-zip
    (zxml/xml1-> :html
                 :body
                 :main)
    z/node
    ((fn splat [element-or-string]
       (if (string? element-or-string)
         element-or-string
         (->> (:content element-or-string)
              (mapcat splat)
              (apply str)))))
    println)
