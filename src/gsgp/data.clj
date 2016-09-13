(ns gsgp.data
  (:require [clojure.java.io :as io]
   :require [clojure.string  :as string]))


(defn load-txt
  [file-name]
  (with-open [r (io/reader file-name)]
    (mapv #(read-string (str "[" % "]")) (line-seq r))))

(defn save-txt
  [file-name data-set]
  (with-open [w (io/writer file-name)]
    (.write w (string/join "\n" (mapv #(string/join " " %) data-set)))))
