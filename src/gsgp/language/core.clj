(ns gsgp.language.core)


(defprotocol ProgramNode
  (program->value [this program-inputs]))


(defrecord Input [index size]
  ProgramNode
  (program->value [this program-inputs] (nth program-inputs (-> this :index))))

(defrecord Constant [value size]
  ProgramNode
  (program->value [this _] (-> this :value)))

(defrecord FunCall [f f-args size]
  ProgramNode
  (program->value [this program-inputs]
    (let [args-results (mapv #(program->value % program-inputs) (-> this :f-args))]
      (apply (-> this :f) args-results))))


(defn constant
  [value]
  (->Constant value 1))

(defn input
  [index]
  (->Input index 1))

(defn funcall
  [f & f-args]
  (->FunCall f (vec f-args) (+ 1 (reduce + (mapv :size f-args)))))


(defrecord ProgramNodeGenerator [name node-arity generator-function])

(defmacro parse-node-generator
  [gdef]
  (if (<= (count gdef) 2)
    (let [[name generator-body] gdef]
      `(->ProgramNodeGenerator ~(str name) 0 (fn [] ~generator-body)))
    (let [[name generator-args generator-body] gdef]
      `(->ProgramNodeGenerator ~(str name) ~(count generator-args) (fn ~generator-args ~generator-body)))))

(defmacro parse-node-generators
  [gdefs]
  (if (empty? gdefs)
    {}
    (let [[gdef & gdefs] gdefs]
      `(let [generators# (parse-node-generators ~gdefs)
             generator# (parse-node-generator ~gdef)]
         (merge {(keyword (-> generator# :name)) generator#} generators#)))))


(defrecord Language [terminals functions mutation crossover])


(defmacro bind-lang-defs
  [gdefs & bodies]
  (if (empty? gdefs)
    `(do ~@bodies)
    (let [[name _ _ :as gdef] (first gdefs)
          gdefs (rest gdefs)]
      `(let [~name (:generator-function (parse-node-generator ~gdef))]
        (bind-lang-defs ~gdefs ~@bodies)))))

(defmacro parse-genetic-operator
  [gdefs [_ op-args op-consts op-body]]
  `(fn []
    (let ~op-consts
      (bind-lang-defs ~gdefs
        (fn ~op-args ~op-body)))))

(defmacro deflang
  [name terminals functions mutation crossover]
  `(do
    (def ~name)
    (let [terminals# (parse-node-generators ~terminals)
          functions# (parse-node-generators ~functions)
          mutation#  (parse-genetic-operator ~functions ~mutation)
          crossover# (parse-genetic-operator ~functions ~crossover)]
      (def ~name
        (->Language terminals# functions# mutation# crossover#)))))

(defn rand-terminal-generator
  [lang]
  (rand-nth (-> lang :terminals vals)))

(defn rand-function-generator
  [lang]
  (rand-nth (-> lang :functions vals)))

(defn rand-program
  [lang max-depth full?]
  (if (or (zero? max-depth)
          (and (not full?) (<= 0.5 (rand))))
    ((-> (rand-terminal-generator lang) :generator-function))
    (let [f-gen (rand-function-generator lang)
          children (vec (repeatedly (-> f-gen :node-arity) #(rand-program lang (dec max-depth) full?)))]
      (apply (-> f-gen :generator-function) children))))
