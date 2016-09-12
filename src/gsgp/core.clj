(ns gsgp.core
  (:gen-class)
  (:require [gsgp.language.core :refer :all])
  (:require [gsgp.world :refer :all])
  (:require [gsgp.selection :refer :all]))

; Testing
(deflang arith
  [(const (constant (- (rand-int 10) 5)))
   (var   (input 0))]

  [(plus  [e1 e2] (funcall + e1 e2))
   (minus [e1 e2] (funcall - e1 e2))
   (times [e1 e2] (funcall * e1 e2))]

  (mutation [t]
    [t1 (rand-program arith 2 false)
     t2 (rand-program arith 2 false)
     s  0.0001]
    (plus t (times (constant s) (minus t1 t2))))

  (crossover [t1 t2]
    [a (rand)
     b (- 1 a)]
    (plus (times (constant a) t1) (times (constant b) t2))))



(def input-set [[0] [1] [2] [3] [4] [5] [6] [7]])

(defn fitness-fn
  [phenotype]
  (/ 1 (+ 1 (/ (Math/sqrt (reduce + (mapv #(* % %) (mapv - phenotype [0 2 5 10 17 26 37 50])))) (count phenotype)))))

(defn selection-fn
  [population c]
  (tournament-selection population 7 c))


(defn -main
  []
  (let [initial-population (vec
                            (repeatedly 400
                              (fn []
                                (let [prog (rand-program arith 2 false)
                                      phenotype (mapv #(program->value prog %) input-set)
                                      fitness (fitness-fn phenotype)]
                                  (->Individual prog phenotype fitness)))))
        world (->SeqWorld initial-population
                          arith
                          fitness-fn
                          selection-fn
                          input-set
                          {:mutation-rate  0.2
                           :crossover-rate 0.3})
        last-world (time (evolve-while world (fn [world gen-n] (<= gen-n 50))))
        best (world-best-individual world)]

    (println
      (:size (:program best))
      (:size (:program (apply max-key #(:size (:program %)) (:population last-world))))
      (mapv #(program->value (:program best) %) (mapv vector (range 8 11))))))
