(ns test.helper
  "Load edn from files while compiling cljs"
  (:require [clojure.edn :as edn]))

(defmacro load-edn
  "Reads a file and returns it as a string.
  It throws an exception if the file is not found."
  [relative-path]
  (edn/read-string (slurp relative-path)))
