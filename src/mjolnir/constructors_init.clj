(ns mjolnir.constructors-init
  (:require [mjolnir.expressions :as exp]
            [mjolnir.types :as tp]
            [clojure.pprint :refer [pprint]]))

(def registered-globals (atom {}))

(defn register-global [ns nm gbl]
  (swap! registered-globals assoc-in [ns nm] gbl))

(defn c-do [& body]
  (exp/->Do body))

(defn gen-binops [op exprs]
  (reduce (partial exp/->Binop op)
          (first exprs)
          (next exprs)))

(defn c-* [& exprs]
  (gen-binops :* exprs))

(defn c-+ [& exprs]
  (gen-binops :+ exprs))

(defn c-- [& exprs]
  (gen-binops :- exprs))

(defn c-div [& exprs]
  (gen-binops :div exprs))

(defn c-mod [& exprs]
  (gen-binops :mod exprs))

(defn c-and [& exprs]
  (gen-binops :and exprs))

#_(defn c-module
  [& body]
  (exp/->Module (name (gensym "module_")) body))

(defn c-if [test then else]
  (exp/->If test then else))


(defn c-fn-t [args ret]
  #_{:post [(tp/valid? %)]}
  (tp/->FunctionType args ret))

(defmacro c-fn [name tp args linkage & body]
  {:pre [name tp args]}
  `(exp/map->Fn {:type ~tp
                 :arg-names ~(mapv clojure.core/name args)
                 :linkage ~linkage
            :name ~name
                 :body (let ~(vec (mapcat (fn [x idx] `[~x
                                                        (exp/->Arg ~idx)])
                                     args
                                     (range)))
                    (if ~(empty? body)
                      nil
                      (c-do ~@body)))}))

(defmacro c-defn [name args & body]
  {:pre [(even? (count args))]}
  (let [args (partition 2 args)
       ret-fn (comp (partial = '->) first)
       ret-type (second (first (filter ret-fn args)))
       args (remove ret-fn args)
       args-map (zipmap (map second args)
                        (range))
        arg-types (mapv first args)
        local-name (clojure.core/name name)
        extern-name (if (string? (:exact (meta name)))
                      (:exact (meta name))
                      (clojure.core/name name))]
    (assert ret-type "No return type given, did you forget the -> type?")
    `(let [nsname# (.getName ~'*ns*)
           ~'_ (defn ~name
         [& args#]
         (exp/->Call (exp/->Gbl (if ~(:exact (meta name))
                                     ~extern-name
                                     (str nsname# "/" ~(clojure.core/name name))))
                     (vec args#)))
           f# (c-fn (if ~(:exact (meta name))
                      ~extern-name
                      (str nsname# "/" ~(clojure.core/name name)))
                   (c-fn-t ~(mapv first args) ~ret-type)
                   ~(mapv second args)
                   ~(when (:extern (meta name)) :extern)
                   ~@body)]
       (register-global nsname# ~local-name f#)
       )))

(defmacro c-const [val arrow tp]
  (assert (= arrow '->) "missing '-> before type")
  `(exp/->Const ~tp ~val))

(defn c-or [& exprs]
  (gen-binops :or exprs))

(defn c-= [a b]
  (exp/->Cmp := a b))

(defn c-< [a b]
  (exp/->Cmp :< a b))

(defn c-> [a b]
  (exp/->Cmp :> a b))

(defn c-<= [a b]
  (exp/->Cmp :<= a b))

(defn c->= [a b]
  (exp/->Cmp :>= a b))

(defn c-not= [a b]
  (exp/->Cmp :not= a b))

(defn c-dec [a]
  (c-+ a -1))

(defn c-inc [a]
  (c-+ a 1))

(defn c-module [includes & body]
  (doto (exp/->Module (-> (reduce (fn [a x]
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
    #_println))

(defn c-aset [arr idx val]
  (exp/->ASet arr idx val))

(defn c-aget [arr idx]
  (exp/->AGet arr idx))

(defn c-nth [arr idx]
  (exp/->Nth arr idx))

(defn c-eget [vec idx]
  (exp/->EGet vec idx))

(defn c-eset [vec idx val]
  (exp/->ESet vec idx val))

(defn c-cast [tp a]
  (exp/->Cast tp a))

(defn c-size-of [tp]
  (exp/->SizeOf tp))

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

(defn c-recur [& items]
  (exp/->Recur (vec items)))

(defmacro c-dotimes [[sym times] & body]
  `(exp/->Loop [[~(name sym) 0]]
              (let [~sym (c-local ~sym)]
                (exp/->Do [~@body
                           (c-if (c-= ~times ~sym)
                                 0
                                 (c-recur (c-+ 1 ~sym)))]))))

(defmacro c-let [bindings & body]
  (reduce (fn [a [local binding]]
            (let [s (name (gensym (str (name local) "_")))]
              `(exp/->Let ~(name local) ~binding 
                         (let [~local (c-local ~local)]
                               ~a))))
           `(exp/->Do ~(vec body))
          (reverse (partition 2 bindings))))

(defn c-malloc
  "Mallocs a instance of the given type"
  [type]
  (exp/->Malloc type))

(defn c-free
  "Constructs an expression that calls free on the given pointer"
  [val]
  (exp/->Free val))

(defmacro c-using [[sym resource] & body]
  `(c-let [~sym ~resource
           ret# (c-do ~@body)]
          (c-free ~sym)
          ret#))


(defn c-struct [name opts]
  (assoc (tp/->StructType name (:extends opts) (:members opts))
    :gc (:gc opts)))

(defn- make-getter [[tp nm]]
  `(defn ~(symbol (str "-" (name nm)))
     [x#]
     (c-get x# ~(-> nm name keyword))))

(defmacro c-defstruct [nm & opts]
  (let [opts (apply hash-map opts)
        extends (:extends opts)
        members (:members opts)
        parted (partition 2 members)]
    `(do (def ~nm (c-struct ~(name nm)
                            ~(merge
                             opts
                             {:extends extends
                              :members (vec (map
                                              (fn [[tp nm]]
                                                [tp (keyword (name nm))])
                                              parted))})))
         ~@(map make-getter
                parted))))

(defn c-set
  "Creates an ->Set expression. ptr is a pointer to a struct, attr is the
   keyword of the member to set and val is value to set the member to."
  [ptr attr val]
  (exp/->Set ptr attr val))

(defn c-get
  "Creates a ->Get expression. ptr is a pointer to a struct and attr is the
   keyword of the member to access."
  [ptr attr]
  (exp/->Get ptr attr))

(defn c-new
  "Constructs a new struct (using malloc), and sets the first (count vals) values
   to the values provide."
  [tp & vals]
  (exp/->New tp vals))

(defn c-global [nm val tp]
  (exp/->Global nm val tp))

(defmacro c-def [nm val arrow tp]
  (assert (= arrow '->) "must include a -> and a type")
  `(let [nsname# (.getName ~'*ns*)]
     (def ~nm (exp/->Gbl (str nsname# "/" ~(name nm))))
     (register-global
      nsname#
      ~(name nm)
      (c-global (str nsname# "/" ~(name nm))
                ~val
                ~tp))))


(defmacro c-for [[var [from to step]] & body]
  `(c-let [to# ~to]
          (c-loop [~var ~from]
                  (c-if (c-< ~var to#)
                        (c-do ~@body
                              (c-recur (c-+ ~var ~step)))
                        ~var))))




;; Black magic is here
(let [ns (create-ns 'mjolnir.constructors)]
  (doseq [[nm var] (ns-publics *ns*)]
    (when (= [\c \-] (take 2 (name nm)))
      (let [nvar (intern ns
                         (symbol (apply str (drop 2 (name nm))))
                         @var)]
        (.setMeta nvar (meta var))
        nvar))))


(defn- constructor [sym]
  (let [sym (if (= (name sym) "/")
              (symbol "div")
              sym)
        s (symbol (str "c-" (name sym)))
        var ((ns-publics (the-ns 'mjolnir.constructors-init)) s)]
    var))

(defn- constructor? [sym]
  (not (nil? (constructor sym))))

(declare convert-form)

(defn- convert-form-1 [x]
  (cond
   (and (seq? x)
        (symbol? (first x))
        (> (count (name (first x))) 2)
        (= (.substring (name (first x)) 0 2) ".-"))
   (convert-form (list 'get
                       (fnext x)
                       (keyword (.substring (name (first x)) 2))))

   (and (seq? x)
        (keyword? (first x)))
   `(exp/->Call
     (exp/->Gbl ~(first x))
     ~(mapv convert-form-1 (next x)))
   
   (seq? x)
   (convert-form x)
   
   (vector? x)
   (into [] (convert-form x))

   (map? x)
   (into {} (convert-form x))

   (set? x)
   (into #{} (convert-form x))
   
   (and (symbol? x)
        (constructor? x))
   (let [s
         (symbol "mjolnir.constructors-init" (str "c-" (if (= (name x) "/")
                                                         "div"
                                                         (symbol x))))]
     s)
   
   
   :else
   x))

(defn- convert-form [body]
  (doall (map convert-form-1 body)))

(defmacro defnf [& body]
  (cons
   'mjolnir.constructors-init/c-defn
   (doall (map convert-form-1 body))))
