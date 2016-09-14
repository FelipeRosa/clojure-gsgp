(ns gsgp.world
  (:require [gsgp.language.core :refer [constant program->value]])
  (:require [gsgp.statistics :refer [covariance variance]]))


(defprotocol World
  "World protocol
  next-generation generates a new world with the next generation of individuals"
  (next-generation [this]))

(defrecord Individual [program phenotype fitness])


(defn world-genetic-operation
  "Returns a individual that results from applying the genetic operation to some parent individuals"
  [world operation & individuals]
  (let [{input-set        :input-set
         fitness-function :fitness-function} world

        operator-f (-> world :language operation)
        operator   (operator-f)

        i-phenotypes (mapv :phenotype individuals)
        i-programs   (mapv :program   individuals)

        j-program   (apply operator i-programs)
        j-phenotype (mapv
                      (fn [i-phenotype inputs]
                        (program->value (apply operator i-phenotype) inputs))
                      (mapv
                        (fn [i-phenotype]
                          (mapv constant i-phenotype))
                        (apply mapv vector i-phenotypes))
                      input-set)
        j-fitness   (fitness-function j-phenotype)]
    (->Individual j-program j-phenotype j-fitness)))

(defn world-mutation
  "Returns the mutation of an individual inside a world"
  [world i]
  (world-genetic-operation world :mutation i))


(defn world-crossover
  "Returns the crossover of two individuals inside a world"
  [world i1 i2]
  (world-genetic-operation world :crossover i1 i2))


(defn world-best-individual
  "Returns a world's individual which has the highest fitness"
  [world]
  (apply max-key :fitness (-> world :population)))

(defn world-average-fitness
  "Returns the average fitness of a world's population"
  [world]
  (/ (reduce + (mapv :fitness (:population world))) (count (:population world))))

(defn world-average-program-size
  "Returns the average program size of a world's population"
  [world]
  (/ (reduce + (mapv #(:size (:program %)) (:population world))) (count (:population world))))


(defn evolve-world
  [world]
  (iterate next-generation world))


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
