(ns gsgp.language.core)


(defprotocol ProgramNode
  (program->value [this program-inputs]))


(defrecord Input [index]
  ProgramNode
  (program->value [this program-inputs] (nth program-inputs (-> this :index))))

(defrecord Constant [value]
  ProgramNode
  (program->value [this _] (-> this :value)))

(defrecord FunCall [f f-args]
  ProgramNode
  (program->value [this program-inputs]
    (let [args-results (mapv #(program->value % program-inputs) (-> this :f-args))]
      (apply (-> this :f) args-results))))


(defn constant
  [value]
  (->Constant value))

(defn input
  [index]
  (->Input index))

(defn funcall
  [f & f-args]
  (->FunCall f (vec f-args)))


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



(defrecord Language [terminals functions])

(defmacro deflang
  [name terminals functions]
  `(let [terminals# (parse-node-generators ~terminals)
         functions# (parse-node-generators ~functions)]
    (def ~name
      (->Language terminals# functions#))))

(defn rand-terminal-generator
  [lang]
  (rand-nth (-> lang :terminals vals)))

(defn rand-function-generator
  [lang]
  (rand-nth (-> lang :functions vals)))

(defn rand-program
  [lang max-depth]
  (if (zero? max-depth)
    ((-> (rand-terminal-generator lang) :generator-function))
    (let [f-gen (rand-function-generator lang)
          children (vec (repeatedly (-> f-gen :node-arity) #(rand-program lang (dec max-depth))))]
      (apply (-> f-gen :generator-function) children))))
