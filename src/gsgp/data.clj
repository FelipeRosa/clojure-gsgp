(ns gsgp.data
  (:require [clojure.java.io :as io]
   :require [clojure.string  :as string]))


(defn load-txt
  "Load a dataset from a .txt file.
  The dataset's columns should be separated by whitespace."
  [file-name]
  (with-open [r (io/reader file-name)]
    (mapv #(read-string (str "[" % "]")) (line-seq r))))

(defn save-txt
  "Save a dataset to a .txt file.
  Dataset columns will be separated by whitespace."
  [file-name data-set]
  (with-open [w (io/writer file-name)]
    (.write w (string/join "\n" (mapv #(string/join " " %) data-set)))))
