(ns gsgp.data
  (:require [clojure.java.io :as io]))


(defn load-txt
  [file-name]
  (with-open [rdr (io/reader file-name)]
    (mapv #(read-string (str "[" % "]")) (line-seq rdr))))
