(ns gsgp.world
  (:require [gsgp.language.core :refer [constant program->value]]))


(defprotocol World
  (next-generation [this])
  (evolve-while [this continue?]))

(defrecord Individual [program phenotype fitness])


(defn world-mutation
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
  [world]
  (apply max-key :fitness (-> world :population)))


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

          p-coefficient 0.001

          select-individual #(selection population p-coefficient)

          p-size (count population)
          m-count (* m-rate p-size)
          c-count (* c-rate p-size)
          s-count (- p-size m-count c-count)

          m-results (vec (repeatedly m-count #(world-mutation this (select-individual))))
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
