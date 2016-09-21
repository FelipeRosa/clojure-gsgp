(ns gsgp.test
  (:require [gsgp.language.core :refer :all])
  (:require [gsgp.world.core :refer :all])
  (:require [gsgp.world.seq :refer :all])
  (:require [gsgp.selection :refer :all])
  (:require [gsgp.data :refer :all])
  (:require [gsgp.statistics :refer :all]))

; Testing
(defn safe-log
  [x]
  (if (<= x 0)
    0
    (Math/log x)))

(defn safe-div
  [a b]
  (if (zero? b)
    1000
    (/ a b)))

(defn safe-exp
  [x]
  (let [r (Math/exp x)]
    (if (Double/isInfinite r)
      1000
      r)))

(deflang arith
  [(const (constant (rand 5)))
   (var   (input (rand-int 6)))]

  [(plus  [e1 e2] (funcall + e1 e2))
   (minus [e1 e2] (funcall - e1 e2))
   (times [e1 e2] (funcall * e1 e2))
   (div   [e1 e2] (funcall safe-div e1 e2))]
  ;  (log   [e1]    (funcall safe-log e1))
  ; (sin   [e1]    (funcall #(Math/sin %) e1))]
  ;  (cos   [e1]    (funcall #(Math/cos %) e1))
  ;  (exp   [e1]    (funcall safe-exp e1))]

  (mutation [t]
    [t1 (rand-program arith 3 false)
     t2 (rand-program arith 3 false)
     s  (rand 0.3)]
    (plus t (times (constant s) (minus t1 t2))))

  (crossover [t1 t2]
    [a (rand)
     b (- 1 a)]
    (plus (times (constant a) t1) (times (constant b) t2))))


(defn main
  [dataset-filename prediction-filename outdata-filename fitness-filename]
  (time
    (let [dataset (load-txt dataset-filename)

          xs (mapv #(take 6 %) dataset)
          ys (mapv last dataset)

          [test-input training-input] (split-at 108 xs)
          [test-output training-output] (split-at 108 ys)

          fitness-fn   (fn [phenotype]
                         (/ 1 (+ 1 (mae phenotype training-output))))
          selection-fn (fn [population c]
                         (tournament-selection population 7 c))

          initial-population (vec
                              (pmap
                                (fn [_]
                                  (let [prog (rand-program arith 2 false)
                                        phenotype (mapv #(program->value prog %) training-input)
                                        fitness (fitness-fn phenotype)]
                                    (->Individual prog phenotype fitness)))
                                (range 1000)))
          world (->SeqWorld initial-population
                            arith
                            fitness-fn
                            selection-fn
                            training-input
                            {:mutation-rate  0.4
                             :crossover-rate 0.3})
          results (take 100
                    (take-while #(do
                                   (println (:fitness (world-best-individual %)))
                                   true)
                      (evolve-world world)))
          last-world (last results)
          fitness-data (mapv #(world-average-fitness %) results)
          best (world-best-individual last-world)

          prediction (mapv #(program->value (:program best) %) test-input)]

      (println
        (:program best)
        (:size (:program (apply max-key #(:size (:program %)) (:population last-world))))
        (rmse test-output prediction))
      (save-txt fitness-filename (mapv vector (range) fitness-data))
      (save-txt prediction-filename (mapv vector (range) prediction))
      (save-txt outdata-filename (mapv vector (range) test-output)))))
