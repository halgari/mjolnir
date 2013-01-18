(ns mjolnir.constructors-init
  (:require [mjolnir.expressions :as exp]
            [mjolnir.types :as tp]))

(def registered-globals (atom {}))

(defn register-global [ns nm gbl]
  (swap! registered-globals assoc-in [ns nm] gbl))

(defn c-do [& body]
  (exp/->Do body))

(defn c-iadd [a b]
  (exp/->IAdd a b))

(defn c-isub [a b]
  (exp/->ISub a b))

#_(defn c-module
  [& body]
  (exp/->Module (name (gensym "module_")) body))

(defn c-if [test then else]
  (exp/->If test then else))


(defn c-fn-t [args ret]
  {:post [(tp/valid? %)]}
  (tp/->FunctionType args ret))

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
           ~'_ (defn ~name
         [& args#]
         (exp/->Call (exp/->Global (str nsname# "/" ~(clojure.core/name name))
                                   (c-fn-t ~(mapv first args) ~ret-type)) (vec args#)))
           f# (c-fn (str nsname# "/" ~(clojure.core/name name))
                   (c-fn-t ~(mapv first args) ~ret-type)
                   ~(mapv second args)
                   ~@body)]
       (register-global nsname# ~(clojure.core/name name) f#)
       )))

(defn c-or [a b]
  (exp/->Or a b))
(defn c-is [a b]
  (exp/->Is a b))

(defn c-module [includes & body]
  (println "g" @registered-globals)
  (doto (exp/->Module "main"
                      (-> (reduce (fn [a x]
                                    (if (namespace x)
                                      (let [exp (get-in @registered-globals
                                                    [(symbol (namespace x))
                                                     (name x)])]
                                        (assert exp (str "Can't find include "
                                                         (symbol (namespace x))
                                                         " "
                                                         (name x)
                                                         " "
                                                         (keys @registered-globals)
                                                         " "
                                                         (keys (@registered-globals
                                                                (symbol (namespace x))))))
                                        (conj a exp))
                                      (concat a
                                              (vals (@registered-globals x)))))
                                  []
                                  includes)
                          (concat body)
                          vec))
    println))

(defn c-aset [arr idx val]
  (exp/->ASet arr (if (vector? idx) idx [idx]) val))

(defn c-aget [arr idx]
  (exp/->AGet arr (if (vector? idx) idx [idx])))

(defmacro c-local [nm]
  `(exp/->Local ~(name nm)))

(defmacro c-loop [binds & body]
  (let [sbinds (partition 2 binds)]
    `(exp/->Loop ~(vec (map (fn [[nm bind]]
                              [(name nm)
                               bind])
                            sbinds))
                 (let [~@(mapcat (fn [[nm _]]
                                   [nm (list 'mjolnir.expressions/->Local (name nm))])
                                 sbinds)]
                   (c-do ~@body)))))

(defmacro c-recur [& items]
  (let [arr (butlast items)
        _ (assert (or (= (last arr) '->)
                      (= (last arr) "->")) "Missing type at end of recur")
        tp (last items)
        items (butlast arr)]
    `(exp/->Recur ~(vec items) ~tp)))

(defmacro c-dotimes [[sym times] & body]
  `(exp/->Loop [[~(name sym) 0]]
              (let [~sym (c-local ~sym)]
                (exp/->Do [~@body
                           (c-if (c-is ~times ~sym)
                                 0
                                 (c-recur (c-iadd 1 ~sym) "->" tp/Int32))]))))

(defmacro c-let [bindings & body]
  (reduce (fn [a [local binding]]
            (let [s (name (gensym (str (name local) "_")))]
              `(exp/->Let ~(name local) ~binding 
                         (let [~local (c-local ~local)]
                               ~a))))
           `(exp/->Do ~(vec body))
          (reverse (partition 2 bindings))))

(defn c-malloc [type cnt]
  (exp/->Malloc type cnt))

(defn c-free [val]
  (exp/->Free val))

(defmacro c-using [[sym resource] & body]
  `(c-let [~sym ~resource
           ret# (c-do ~@body)]
          (c-free ~sym)
          ret#))

;; Black magic is here
(let [ns (create-ns 'mjolnir.constructors)]
  (doseq [[nm var] (ns-publics *ns*)]
    (when (= [\c \-] (take 2 (name nm)))
      (let [nvar (intern ns
                         (symbol (apply str (drop 2 (name nm))))
                         @var)]
        (.setMeta nvar (meta var))
        nvar))))


