(ns gsgp.world.seq
  (:require [gsgp.world.core :refer [World world-mutation world-crossover]])
  (:require [gsgp.statistics :refer [covariance variance]]))


(defrecord SeqWorld
  [population
   language
   fitness-function
   selection-function
   input-set
   parameters]

  World
  (next-generation
    [this]
    (let [{m-rate :mutation-rate
           c-rate :crossover-rate} (:parameters this)

          {selection :selection-function
           population :population} this

          program-sizes     (mapv #(:size (:program %)) population)
          program-fitnesses (mapv :fitness population)
          p-coefficient     (/ (covariance program-sizes program-fitnesses) (variance program-sizes))

          select-individual #(selection population p-coefficient)

          p-size  (count population)
          m-count (* m-rate p-size)
          c-count (* c-rate p-size)
          s-count (- p-size m-count c-count)

          m-results (mapv #(world-mutation this %) (take m-count (shuffle population)))
          c-results (vec (repeatedly c-count #(world-crossover this (select-individual) (select-individual))))
          s-results (vec (repeatedly s-count select-individual))]
      (update this :population
        (constantly
          (vec (concat m-results c-results s-results)))))))
