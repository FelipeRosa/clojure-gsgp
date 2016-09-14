(ns gsgp.statistics)


(defn covariance
  "Compute the covariance between two random variables
  xs and ys should be vectors"
  [xs ys]
  (let [n (count xs)
        mean-x (/ (reduce + xs) n)
        mean-y (/ (reduce + ys) n)]
    (/ (reduce + (mapv #(* (- %1 mean-x) (- %2 mean-y)) xs ys)) n)))

(defn variance
  "Compute the variace of a random variable"
  [xs]
  (covariance xs xs))


(defn mae
  "Returns the mean absolute error between
  a vector of predicted values and a vector of known outcomes"
  [predicted-values outcomes]
  (/ (reduce + (mapv #(Math/abs %) (mapv - predicted-values outcomes))) (count outcomes)))

(defn mse
  "Return the mean squared error between
  a vector of predicted values and a vector of known outcomes"
  [predicted-values outcomes]
  (/ (reduce + (mapv #(* % %) (mapv - predicted-values outcomes))) (count outcomes)))

(defn rmse
  "Returns the root-mean-square error between
  a vector of predicted values and a vector of known outcomes"
  [predicted-values outcomes]
  (Math/sqrt (mse predicted-values outcomes)))
