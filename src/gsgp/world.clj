(ns gsgp.world
  (:require [gsgp.language.core :refer [constant program->value]])
  (:require [gsgp.statistics :refer [covariance variance]]))


(defprotocol World
  "World protocol
  next-generation generates a new world with the next generation of individuals
  evolve-while computes generations until continue? is false"
  (next-generation [this])
  (evolve-while [this continue?]))

(defrecord Individual [program phenotype fitness])


(defn world-mutation
  "Returns the mutation of an individual inside a world"
  [world i]
  (let [{mutation-f :mutation} (:language world)
        {input-set        :input-set
         fitness-function :fitness-function} world

        {i-program :program
         i-phenotype :phenotype} i

        i-mut (mutation-f)

        i1-program (i-mut i-program)
        i1-phenotype (mapv #(program->value (i-mut (constant %1)) %2) i-phenotype input-set)
        i1-fitness (fitness-function i1-phenotype)]
    (->Individual i1-program i1-phenotype i1-fitness)))


(defn world-crossover
  "Returns the crossover of two individuals inside a world"
  [world i1 i2]
  (let [{crossover-f :crossover} (:language world)
        {input-set        :input-set
         fitness-function :fitness-function} world

        {i1-program   :program
         i1-phenotype :phenotype} i1

        {i2-program   :program
         i2-phenotype :phenotype} i2

        i-cross (crossover-f)

        i-program (i-cross i1-program i2-program)
        i-phenotype (mapv #(program->value (i-cross (constant %1) (constant %2)) %3) i1-phenotype i2-phenotype input-set)
        i-fitness (fitness-function i-phenotype)]
    (->Individual i-program i-phenotype i-fitness)))

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
          (vec (concat m-results c-results s-results))))))

  (evolve-while
    [this continue?]
    (loop [gen-n 1
           world this]
     (if (continue? world gen-n)
       (recur (inc gen-n) (next-generation world))
       world))))
