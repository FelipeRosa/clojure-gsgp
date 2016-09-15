(ns gsgp.world.core
  (:require [gsgp.language.core :refer [constant program->value]]))


(defprotocol World
  "World protocol
  next-generation generates a new world with the next generation of individuals"
  (next-generation [this]))

(defrecord Individual [program phenotype fitness])


(defn evolve-world
  [world]
  (iterate next-generation world))


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
