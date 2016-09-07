(ns gsgp.core
  (:require [gsgp.language.core :refer :all]))


; Testing
(deflang arith
  [(const (constant (- (rand-int 10) 5)))
   (var   (input (rand-int 2)))]

  [(plus  [e1 e2] (funcall + e1 e2))
   (times [e1 e2] (funcall * e1 e2))])

(deflang bool
  [(const (constant (rand-nth [true false])))
   (var   (input (rand-int 2)))]

  [(not [e1]    (funcall not e1))
   (and [e1 e2] (funcall (macroexpand 'and) e1 e2))
   (or  [e1 e2] (funcall (macroexpand 'or)  e1 e2))])


(defn teste
  []
  (let [p (rand-program bool 3)
        inputs [true false false]]
    (program->value p inputs)))
