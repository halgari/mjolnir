(ns examples.simple-lisp
  #_(:gen-class)
  (:require
   [mjolnir.config :as config]
   [mjolnir.types :refer :all]
   [mjolnir.expressions :refer :all]
   [mjolnir.constructors-init :as costructors-init]
   [clojure.pprint :refer [pprint]]
   [mjolnir.targets.target :refer [emit-to-file]])
  (:alias c mjolnir.constructors))

;; Type System

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

(c/defn ^{:exact "GC_malloc"} ^:extern GC_malloc [Int64 size -> Int8*])
(c/defn ^{:exact "GC_init"} ^:extern GC_init [-> Int8*])

(defn tmalloc [tp]
  (c/bitcast (GC_malloc (->SizeOf tp))
             (->PointerType tp)))

(def WObject* (->PointerType WObject))
(def WInt64* (->PointerType WInt64))
(def WString* (->PointerType WString))
(def WFn* (->PointerType WFn))

(def Int64-Type 1)
(def WString-Type 2)
(def WFn-Type 3)


;; Each Fn gets handed itself as the first argument, thus the arg
;; list item counts seem to be off by one

(def TNullaryFn (c/fn-t [WObject*] WObject*))
(def TUnaryFn (c/fn-t [WObject* WObject*] WObject*))
(def TBinaryFn (c/fn-t [WObject* WObject* WObject*] WObject*))

(def TNullaryFn* (->PointerType TNullaryFn))
(def TUnaryFn* (->PointerType TUnaryFn))
(def TBinaryFn* (->PointerType TBinaryFn))

(def argc->fn-t
  [TNullaryFn
   TUnaryFn
   TBinaryFn])

;; Wrapping/unwrapping
 
#_(c/defn wrap-WInt64 [Int64 v -> WObject*]
  (-> (tmalloc WInt64)
      (c/set :type Int64-type)
      (c/set :int64-value v)
      (c/bitcast WObject*)))

(defn- symstr [& more]
  (symbol (apply str (map str more))))

(defn gen-wrap-fn [[wtp tpid attr vtp]]
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

(wrap-types [[WInt64 1 :int64-value Int64]
             [WString 2 :str-value Int8*]
             [WFn 3 :fn-value Int8*]])

(c/defn Int64-+ [WObject* self WObject* a WObject* b -> WObject*]
  (wrap-WInt64 (c/+ (unwrap-WInt64 a)
                    (unwrap-WInt64 b))))

(c/defn Int64-- [WObject* self WObject* a WObject* b -> WObject*]
  (wrap-WInt64 (c/+ (unwrap-WInt64 a)
                    (unwrap-WInt64 b))))

(c/defn Int64-div [WObject* self WObject* a WObject* b -> WObject*]
  (wrap-WInt64 (c/+ (unwrap-WInt64 a)
                    (unwrap-WInt64 b))))

(c/defn Int64-* [WObject* self WObject* a WObject* b -> WObject*]
  (wrap-WInt64 (c/+ (unwrap-WInt64 a)
                    (unwrap-WInt64 b))))

(c/defn WFn-invoke0 [WObject* fno -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TNullaryFn*) [fno]))

(c/defn WFn-invoke1 [WObject* fno WObject* a -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TUnaryFn*) [fno a]))

(c/defn WFn-invoke2 [WObject* fno WObject* a WObject* b -> WObject*]
  (->CallPointer (c/bitcast (unwrap-WFn fno) TBinaryFn*) [fno a b]))



(def arg-dispatch
  [WFn-invoke0
   WFn-invoke1
   WFn-invoke2])

(defn invoke [f & args]
  (apply (arg-dispatch (count args)) f args))

(def ^:dynamic globals (atom {}))
(def ^:dynamic locals)

(defn wrap-global-fn [f tp]
  (wrap-WFn (c/bitcast (->GetGlobal f tp) Int8*)))



(def sym-maps
  {'+ (wrap-global-fn ::Int64-+ TBinaryFn)})


(c/defn ^:exact main [-> Int64]
  (GC_init)
  (unwrap-WInt64 (WFn-invoke0 (wrap-global-fn :-run TNullaryFn))))

#_(c/defn WFn-test [-> TBinaryFn]
  (c/bitcast (unwrap-WFn (sym-maps '+)) TBinaryFn))

(defn resolve-symbol [x]
  (let [r (or (locals x)
              (@globals x)
              (sym-maps x))]
    (assert r (str "Could not resolve: " x))
    r))

(def builtins #{'defn})

(defmulti compile-builtin (fn [f & args]
                            (keyword (name f))))

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

(defmethod compile-builtin :defn
  [_ nm args & body]
  (let [fn-t (argc->fn-t (count args))
        new-global (wrap-global-fn nm fn-t)]
    (swap! globals assoc nm new-global)
    (->Fn (name nm)
          (argc->fn-t (count args))
          (concat ["this"]
                  (map name args))
          (binding [locals (merge
                            {nm (wrap-global-fn nm fn-t)}
                            (zipmap
                             args
                             (map #(->Argument % WObject*) (range 1 (inc (count args))))))]
            (->Do (mapv compile-item body))))))



(defmethod compile-item :symbol
  [x]
  (resolve-symbol x))

(defmethod compile-item :int
  [x]
  (wrap-WInt64 x))

(defmethod compile-item :call
  [[f & args]]
  (apply
   invoke
   (resolve-symbol f)
   (map compile-item args)))

(defmethod compile-item :builtin
  [itm]
  (apply compile-builtin itm))

(comment
  (def fib [x]
    (+ x 1)))

(defn compile-lisp [data]
  (let [d (->>
           (mapv compile-item data)
           (apply c/module ['examples.simple-lisp])
           #_debug
           build
           optimize)]
    (dump d)
    (emit-to-file (config/default-target)
                  d
                  {:filename "simple-lisp.s"
                   :obj-type :asm})))

(defn -main []
  (compile-lisp '[(defn -run []
                    (+ 21 21))])
  0)



