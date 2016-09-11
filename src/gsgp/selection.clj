(ns gsgp.selection)


(defn tournament-selection
  [population tournament-size pressure-coefficient]
  (let [participants (take tournament-size population)
        adjusted-fitness (fn [i] (- (:fitness i) (* pressure-coefficient (:size (:program i)))))]
    (apply max-key adjusted-fitness participants)))
