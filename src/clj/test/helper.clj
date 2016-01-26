(ns test.helper
  "Load edn from files while compiling cljs"
  (:require [clojure.edn :as edn]))

(defmacro load-edn
  "Reads a file and returns it as a string"
  [relative-path]
  (edn/read-string (slurp relative-path)))
