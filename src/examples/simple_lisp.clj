(ns examples.simple-lisp
  (:gen-class)
  (:require
   [mjolnir.config :as config]
   [mjolnir.types :refer :all]
   [mjolnir.expressions :refer :all]
   [mjolnir.constructors-init :as costructors-init]
   [clojure.pprint :refer [pprint]]
   [mjolnir.targets.target :refer [emit-to-file]])
  (:alias c mjolnir.constructors))

;; This is an example of an extremely simple lisp-like compiler built
;; using Mjolnir. This is not feature complete, and most likely will
;; never be, but it serves as an example of how more complex systems
;; could be developed in Mjolnir.

;; Type System
;; Our type system is simply a set of structs. We'll have a typeid in
;; the parent object, and each child struct will have one or more
;; value members

(c/defstruct WObject
  :members [Int64 type])

(c/defstruct WInt64
  :extends WObject
  :members [Int64 int64-value])

(c/defstruct WString
  :extends WObject
  :members [Int8* str-value])

(c/defstruct WFn
  :extends WObject
  :members [Int8* fn-value])



;; Defs from external libs. Mjolnir doesn't support varargs yet, so we
;; just have one version of print-Int64
(c/defn ^{:exact "GC_malloc"} ^:extern GC_malloc [Int64 size -> Int8*])
(c/defn ^{:exact "GC_init"} ^:extern GC_init [-> Int8*])
(c/defn ^{:exact "printf"} ^:extern print-Int64 [Int8* format Int64 val -> Int64 ])


;; Simple constructor fn for allocating a struct from GC memory
(defn tmalloc [tp]
  (c/bitcast (GC_malloc (->SizeOf tp))
             (->PointerType tp)))

;; Define some helper pointer types 
(def WObject* (->PointerType WObject))
(def WObject** (->PointerType WObject*))
(def WObject*** (->PointerType WObject**))
(def WInt64* (->PointerType WInt64))
(def WString* (->PointerType WString))
(def WFn* (->PointerType WFn))

;; Simple TypeIDs
(def Int64-Type 1)
(def WString-Type 2)
(def WFn-Type 3)

;; We're going to cache ints. So instead boxing common ints everytime
;; we need a boxed in, we will instead cache these ints. This will
;; save an memory allocation in these cases. 
(c/def WInt64-cache nil -> WObject***)


;; Each Fn gets handed itself as the first argument, thus the arg
;; list item counts seem to be off by one. That being known let's help
;; ourselves a bit by providing names for three well know arities

(def TNullaryFn (c/fn-t [WObject*] WObject*))
(def TUnaryFn (c/fn-t [WObject* WObject*] WObject*))
(def TBinaryFn (c/fn-t [WObject* WObject* WObject*] WObject*))

(def TNullaryFn* (->PointerType TNullaryFn))
(def TUnaryFn* (->PointerType TUnaryFn))
(def TBinaryFn* (->PointerType TBinaryFn))

;; We can use a vector as a int to fn type converter
(def argc->fn-t
  [TNullaryFn
   TUnaryFn
   TBinaryFn])

;; Wrapping/unwrapping

;; We're about to create common wrap/unwrap functions, using macros.
;; Once we're done, we're going to have a set of functions that look
;; something like this:
 
#_(c/defn wrap-WInt64 [Int64 v -> WObject*]
  (-> (tmalloc WInt64)
      (c/set :type Int64-type)
      (c/set :int64-value v)
      (c/bitcast WObject*)))

(defn- symstr
  "Like str but the end result is a symbol"
  [& more]
  (symbol (apply str (map str more))))

(defn gen-wrap-fn
  "Generates a wrap function given a wrapped type, a type id, and the
   name and type of the value to be wrapped"
  [[wtp tpid attr vtp]]
  `(c/defn ~(symstr "wrap-" wtp) [~vtp v# ~'-> WObject*]
     (c/let [a# (tmalloc ~wtp) ]
       (-> a#
        (c/set :type ~tpid)
        (c/set ~attr v#)
        (c/bitcast WObject*)))))

(defn debug [x]
  (pprint x)
  x)

(defn gen-unwrap-fn [[wtp tpid attr vtp]]
  `(c/defn ~(symstr "unwrap-" wtp) [WObject* v# ~'-> ~vtp]
     (c/get (c/bitcast v# (->PointerType ~wtp)) ~attr)))  




(defmacro wrap-types [tps]
  (list* 'do
         (mapcat
          (fn [tp]
            [(gen-wrap-fn tp)
             (gen-unwrap-fn tp)])
          tps)))

;; Use the macros to generate the wrap/unwrap functions
(wrap-types [[WInt64 1 :int64-value Int64]
             [WString 2 :str-value Int8*]
             [WFn 3 :fn-value Int8*]])


;; Instead of using wrap-WInt64, we can use cached-WInt64, this will
;; result in faster creation of wrapped integers for values from -31
;; to 991
(c/defn cached-WInt64 [Int64 x -> WObject*]
  (c/let [drc (c/aget WInt64-cache 0)]
         (c/if (c/< x 992)
               (c/if (c/> x -32)
                     (c/aget drc
                             (c/+ x 32))
                     (wrap-WInt64 x))
               (wrap-WInt64 x))))

;; Addition of two WInt64
(c/defn WInt64-+ [WObject* self WObject* a WObject* b -> WObject*]
  (cached-WInt64 (c/+ (unwrap-WInt64 a)
                      (unwrap-WInt64 b))))

;; Comparison of two Int64s
(c/defn WInt64-= [WObject* self WObject* a WObject* b -> WObject*]
  (c/if (c/is (unwrap-WInt64 a)
              (unwrap-WInt64 b))
        (cached-WInt64 1)
        (cached-WInt64 0)))

;; Print a WInt64 via libc's printf 
(c/defn WInt64-print [WObject* self WObject* a -> WObject*]
  (cached-WInt64 (print-Int64 (c/const "%i\n" -> Int8*)
                            (unwrap-WInt64 a))))

;; Given a WFn, invoke it with no args
(c/defn WFn-invoke0 [WObject* fno -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TNullaryFn*) [fno]))

;; Given a WFn, invoke it with one arg
(c/defn WFn-invoke1 [WObject* fno WObject* a -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TUnaryFn*) [fno a]))

;; Given a WFn, invoke it with two args
(c/defn WFn-invoke2 [WObject* fno WObject* a WObject* b -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TBinaryFn*) [fno a b]))

;; Dispatch to a invoke based on a given argc
(def arg-dispatch
  [WFn-invoke0
   WFn-invoke1
   WFn-invoke2])

;; Generate a call site for a given number of args
(defn invoke [f & args]
  (apply (arg-dispatch (count args)) f args))

;; These will hold known symbols during compilation
(def ^:dynamic globals (atom {}))
(def ^:dynamic locals)

;; Generate code that wraps a global fn and returns a WFn
(defn wrap-global-fn [f tp]
  (wrap-WFn (c/bitcast (->GetGlobal f tp) Int8*)))


;; These are known global fns, we provide two ways to call each
;; function. Direct is used if we are calling the function directly,
;; indirect calling is used when we only have a function pointer. The
;; direct method is most commonly used when the function being called
;; is referenced by name as the first item in a s-expression. 
(def sym-maps
  {'+ {:indirect (wrap-global-fn ::WInt64-+ TBinaryFn)
       :direct [::WInt64-+ TBinaryFn]}
   '= {:indirect (wrap-global-fn ::WInt64-= TBinaryFn)
       :direct [::WInt64-= TBinaryFn]}
   'print {:indirect (wrap-global-fn ::WInt64-print TUnaryFn)
           :direct [::WInt64-print TUnaryFn]}})


;; This is global init code for the entire runtime.
(c/defn init-all [-> Int64]
  (c/let [cache (c/bitcast (GC_malloc 8192)
                           WObject**)]
         (->Store WInt64-cache cache)
         (c/dotimes [x 1024]
                    (c/aset cache
                            x
                            (wrap-WInt64 (c/+ x -32))))))

;; This is the main entry point for a simple-lisp program. First, init
;; th GC, then init our internal runtime, then invoke the -run function
(c/defn ^:exact main [-> Int64]
  (GC_init)
  (init-all)
  (unwrap-WInt64 (WFn-invoke0 (wrap-global-fn :-run TNullaryFn))))

#_(c/defn WFn-test [-> TBinaryFn]
  (c/bitcast (unwrap-WFn (sym-maps '+)) TBinaryFn))

;; Given a symbol, try to find where it what it would refer to given
;; the current context. 
(defn resolve-symbol [x]
  (let [r (or (locals x)
              (@globals x)
              (sym-maps x))]
    (assert r (str "Could not resolve: " x))
    r))

;; These refer to specal forms, or builtins
(def builtins #{'defn 'if})

;; Dispatch to a given builtin handler
(defmulti compile-builtin (fn [f & args]
                            (keyword (name f))))

;; Dispatch to an appropriate handler 
(defmulti compile-item (fn [x]
                         (cond
                          (integer? x) :int
                          (string? x) :string
                          (symbol? x) :symbol
                          (and (seq? x)
                               (symbol? (first x))
                               (builtins (first x))) :builtin
                          (and (seq? x)
                               (symbol? (first x))) :call
                               :else :sexp)))

;; Define a new global function. 
(defmethod compile-builtin :defn
  [_ nm args & body]
  (let [fn-t (argc->fn-t (count args))
        new-global {:indirect (wrap-global-fn (name nm) fn-t)
                    :direct [(name nm) fn-t]}]
    (swap! globals assoc nm new-global)
    (->Fn (name nm)
          (argc->fn-t (count args))
          (cons "self" (map name args))
          (binding [locals (merge
                            {nm new-global}
                            (zipmap
                             args
                             (map #(->Argument % WObject*)
                                  (range 1 (inc (count args))))))]
            (->Do (mapv compile-item body))))))

(defmethod compile-builtin :if
  [_ test then else]
  (c/if (c/is (unwrap-WInt64 (compile-item test))
              0)
        (compile-item else)
        (compile-item then)))

(defmethod compile-item :symbol
  [x]
  (resolve-symbol x))

(defmethod compile-item :int
  [x]
  (cached-WInt64 x))

(defmethod compile-item :call
  [[f & args]]
  (let [resolved (resolve-symbol f)]
    ;; Try to get a direct way to call the function, otherwise, fall
    ;; back to the slower method of using wrapped functions
    (if (and (map? resolved)
             (:direct resolved))
      (->Call (apply ->GetGlobal (:direct resolved))
              (concat [(c/const nil -> WObject*)] (mapv compile-item args)))
      (apply
       invoke
       (or (and (map? resolved)
                (:indirect resolved))
           resolved)
       (mapv compile-item args)))))

(defmethod compile-item :builtin
  [itm]
  (apply compile-builtin itm))

(defn compile-lisp [data]
  
    (let [_ (print "Creating Module: ")
          module (time (->>
                        (mapv compile-item data)
                        (apply c/module ['examples.simple-lisp])))
          _ (print "Building Module: ")
          built (time (build module))
          _ (print "Optimizing: ")
          optimized (time (optimize built))]
      optimized))

(defn -main [program & opts]
  (binding [config/*float-type* Float32
            config/*int-type* Int64
            config/*target* (config/default-target)]
    (let [{:keys [filename obj-type]} (apply hash-map (map read-string opts))
          exprs (read-string (str "[" (slurp program) "]"))
          optimized (compile-lisp exprs)]
      (assert (and filename obj-type) ":obj-type and :filename are required")
      
      (print "Writing Output file: ")
      (time (emit-to-file config/*target*
                          optimized
                          {:filename filename
                           :obj-type obj-type}))))
  (println "Finished")
  (shutdown-agents)
  0)



