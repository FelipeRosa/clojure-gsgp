(ns gsgp.statistics)


(defn covariance
  [xs ys]
  (let [n (count xs)
        mean-x (/ (reduce + xs) n)
        mean-y (/ (reduce + ys) n)]
    (/ (reduce + (doall (map #(* (- %1 mean-x) (- %2 mean-y)) xs ys))) n)))

(defn variance
  [xs]
  (covariance xs xs))


(defn diff
  [predicted-values training-output]
  (reduce + (doall (map #(Math/abs %) (map - predicted-values training-output)))))

(defn rmse
  [predicted-values training-output]
  (Math/sqrt (/ (reduce + (mapv #(* % %) (mapv - predicted-values training-output))) (count training-output))))
