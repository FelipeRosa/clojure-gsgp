(ns gsgp.world.seq
  (:require [gsgp.world.core :refer [World world-mutation world-crossover]])
  (:require [gsgp.statistics :refer [covariance variance]]))


(defn- population-p-coeffiecient
  [population]
  (let [program-sizes     (mapv #(:size (:program %)) population)
        program-fitnesses (mapv :fitness population)]
    (/ (covariance program-sizes program-fitnesses) (+ 1 (variance program-sizes)))))


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

          p-coefficient     (population-p-coeffiecient population)
          select-individual #(selection population p-coefficient)

          p-size  (count population)
          m-count (* m-rate p-size)
          c-count (* c-rate p-size)

          m-results (mapv #(world-mutation this %) (take m-count (shuffle population)))
          c-results (vec (repeatedly c-count #(world-crossover this (select-individual) (select-individual))))

          intermediate-population    (vec (concat population m-results c-results))
          intermediate-p-coefficient (population-p-coeffiecient intermediate-population)
          select-next-individual #(selection intermediate-population intermediate-p-coefficient)

          next-population (vec (repeatedly p-size #(select-next-individual)))]
      (update this :population (constantly next-population)))))
