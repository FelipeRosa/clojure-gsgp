(ns gsgp.selection)


(defn tournament-selection
  "Runs a round of tournament selection.
  population should be a vector of individuals
  tournament-size is the amount of individuals to be chosen to participated the tournament
  pressure-coefficient is a constant used to control bloat."
  [population tournament-size pressure-coefficient]
  (let [participants (take tournament-size (shuffle population))
        adjusted-fitness (fn [i] (- (:fitness i) (* pressure-coefficient (:size (:program i)))))]
    (apply max-key adjusted-fitness participants)))
