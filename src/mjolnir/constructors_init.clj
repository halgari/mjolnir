(ns mjolnir.constructors-init
  (:require [mjolnir.expressions :as exp]
            [mjolnir.types :as tp]))

(def registered-globals (atom {}))

(defn register-global [ns nm gbl]
  (swap! registered-globals assoc-in [ns nm] gbl))


(defn c-module
  [& body]
  (exp/->Module (name (gensym "module_")) body))

(defn c-if [test then else]
  (exp/->If test then else))

(defn c-fn-t [args ret]
  {:post [(tp/valid? %)]}
  (tp/map->FunctionType args ret))

(defmacro c-fn [name tp args & body]
  {:pre [name tp args]}
  `(exp/map->Fn {:type ~tp
            :arg-names ~(mapv clojure.core/name args)
            :name ~name
            :body (let ~(vec (mapcat (fn [x idx] [x (exp/->Argument idx)])
                                     args
                                     (range)))
                    (c-do ~@body))}))



(defmacro c-defn [name args & body]
  (let [args (partition 2 args)
       ret-fn (comp (partial = '->) first)
       ret-type (second (first (filter ret-fn args)))
       args (remove ret-fn args)
       args-map (zipmap (map second args)
                        (range))
        arg-types (mapv first args)]
    `(let [nsname# (.getName ~'*ns*)
           f# (c-fn (str nsname# "/" ~(clojure.core/name name))
                   (c-fn-t ~(mapv first args) ~ret-type)
                   ~(mapv second args)
                   ~@body)]
       (register-global nsname# ~(clojure.core/name name) f#)
       (defn ~name
         [& args#]
         (exp/->Call f# (vec args#))))))

(defn c-or [a b]
  nil)
(defn c-is [a b]
  nil)

;; Black magic is here
(let [ns (create-ns 'mjolnir.constructors)]
  (doseq [[nm var] (ns-publics *ns*)]
    (when (= [\c \-] (take 2 (name nm)))
      (let [nvar (intern ns
                         (symbol (apply str (drop 2 (name nm))))
                         @var)]
        (.setMeta nvar (meta var))
        nvar))))
