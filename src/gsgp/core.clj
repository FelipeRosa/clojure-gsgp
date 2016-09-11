(ns gsgp.core
  (:require [gsgp.language.core :refer :all]))


; Testing
(deflang arith
  [(const (constant (- (rand-int 10) 5)))
   (var   (input (rand-int 2)))]

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


(defn teste
  []
  (let [p (rand-program arith 3 false)
        inputs [1 2 3]]
    (program->value p inputs)))
