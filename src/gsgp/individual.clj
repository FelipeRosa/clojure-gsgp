(ns gsgp.individual
  (:require [gsgp.language.core :refer [program->value]]))


(defrecord Individual [program phenotype fitness])


(defn program->phenotype
  [program input-set]
  (mapv #(program->value program %) input-set))
