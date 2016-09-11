(ns gsgp.selection)


(defn tournament-selection
  [population tournament-size pressure-coefficient]
  (let [participants (take tournament-size population)
        adjusted-ftness (fn [i] (- (:fitness i) (:size (:program i))))]))
