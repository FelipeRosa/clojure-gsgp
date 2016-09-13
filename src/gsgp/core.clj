(ns gsgp.core
  (:gen-class)
  (:require [gsgp.language.core :refer :all])
  (:require [gsgp.world :refer :all])
  (:require [gsgp.selection :refer :all])
  (:require [gsgp.data :refer :all])
  (:require [gsgp.statistics :refer :all]))

; Testing
(deflang arith
  [(const (constant (rand-int 10)))
   (var   (input (rand-int 13)))]

  [(plus  [e1 e2] (funcall + e1 e2))
   (minus [e1 e2] (funcall - e1 e2))
   (times [e1 e2] (funcall * e1 e2))]

  (mutation [t]
    [t1 (rand-program arith 3 false)
     t2 (rand-program arith 3 false)
     s  0.1]
    (plus t (times (constant s) (minus t1 t2))))

  (crossover [t1 t2]
    [a (rand)
     b (- 1 a)]
    (plus (times (constant a) t1) (times (constant b) t2))))


(defn main
  [dataset-filename prediction-filename outdata-filename]
  (time
    (let [dataset (load-txt dataset-filename)

          xs (mapv #(take 13 %) dataset)
          ys (mapv last dataset)

          [test-input training-input] (split-at 156 xs)
          [test-output training-output] (split-at 156 ys)

          fitness-fn   (fn [phenotype]
                         (/ 1 (+ 1 (rmse phenotype training-output))))
          selection-fn (fn [population c]
                         (tournament-selection population 5 c))

          initial-population (vec
                              (repeatedly 1000
                                (fn []
                                  (let [prog (rand-program arith 2 false)
                                        phenotype (mapv #(program->value prog %) training-input)
                                        fitness (fitness-fn phenotype)]
                                    (->Individual prog phenotype fitness)))))
          world (->SeqWorld initial-population
                            arith
                            fitness-fn
                            selection-fn
                            training-input
                            {:mutation-rate  0.4
                             :crossover-rate 0.2})
          last-world (evolve-while world (fn [world gen-n] (let [fit (:fitness (world-best-individual world))] (println gen-n fit) (<= gen-n 20))))
          best (world-best-individual world)

          prediction (mapv #(program->value (:program best) %) test-input)]

      (println
        (:size (:program best))
        (:size (:program (apply max-key #(:size (:program %)) (:population last-world))))
        (rmse test-output prediction))
      (save-txt prediction-filename (mapv vector (range) prediction))
      (save-txt outdata-filename (mapv vector (range) test-output)))))
