(ns mjolnir.expressions
  (:require [mjolnir.types :refer :all])
  (:require [mjolnir.llvmc :as llvm])
  (:import [com.sun.jna Native Pointer]))

(def ^:dynamic *builder*)
(def ^:dynamic *module*)
(def ^:dynamic *fn*)
(def ^:dynamic *llvm-fn*)
(def ^:dynamic *locals* {})
(def ^:dynamic *llvm-locals*)
(def ^:dynamic *llvm-recur-point*)
(def ^:dynamic *llvm-recur-phi*)
(def ^:dynamic *llvm-recur-block*)
(def ^:dynamic *block* (atom nil))
(def ^:dynamic *recur-point*)

(def genname (comp name symbol))

(defprotocol Expression
  (return-type [this])
  (build [this]))


(defn Expression? [this]
  (extends? Expression (type this)))


(defrecord ConstInteger [value type]
  Validatable
  (validate [this]
    (assure (integer? value))
    (assure (type? type)))
  Expression
  (return-type [this]
    type)
  (build [this]
    (llvm/ConstInt (llvm-type type) value true)))



(defrecord Add [a b]
  Validatable
  (validate [this]
    (validate-all a b)
    (assure-same-type a b))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildAdd *builder* (build a) (build b))))

(defrecord Or [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (Expression? a)
    (Expression? b))
  Expression
  (return-type [this]
    Int1)
  (build [this]
    (llvm/BuildOr *builder*
                  (build a)
                  (build b)
                  (genname "or_"))))

(defrecord Is [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (Expression? a)
    (Expression? b)
    (assure-same-type (return-type a) (return-type b)))
  Expression
  (return-type [this]
    Int1)
  (build [this]
    (llvm/BuildICmp *builder* llvm/LLVMIntEQ (build a) (build b) (genname "is_"))))

(defprotocol IFunctionExpression
  (argument [this idx] "Get an expression for the given argument"))

(defprotocol NamedExpression
  (get-name [this]))

(defrecord Argument [idx]
  Validatable
  (validate [this]
    (assure (argument *fn* idx)))
  Expression
  (return-type [this]
    (return-type (argument *fn* idx)))
  (build [this]
    (build (argument *fn* idx))))

(defrecord FnArgument [arg-name arg-idx arg-type]
    Validatable
    (validate [this]
      (assure (string? arg-name))
      (assure (type? arg-type))
      (assure (integer? arg-idx)))
    Expression
    (return-type [this]
      arg-type)
    (build [this]
      (llvm/GetParam *llvm-fn* arg-idx)))




(defprotocol GlobalExpression
  (stub-global [this]))

(defn GlobalExpression? [exp]
  (extends? GlobalExpression (type exp)))

(defrecord Fn [name type arg-names body]
  IFunctionExpression
  (argument [this idx]
    (FnArgument. (nth arg-names idx)
                 idx
                 (nth (:arg-types type) idx)))
  Validatable
  (validate [this]
    (binding [*fn* this]
      (assure (string? name))
      (assure-type type)
      (assure (FunctionType? type))
      (assure (every? string? arg-names))
      (assure (Expression? body))
      (assure (= (count (:arg-types type)) (count arg-names)))
      (valid? body)
      (assure-same-type (return-type body) (:ret-type type))
      (when-let [linkage (:linkage this)]
        (assure (llvm/kw->linkage linkage)))))
  Expression
  (return-type [this]
    (return-type type))
  (build [this]
    (when body
      (let [fnc (llvm/GetNamedFunction *module* name)
            newargs (into {} (map (fn [s idx]
                                    [s (llvm/GetParam fnc idx )])
                                  arg-names
                                  (range (count arg-names))))]
        (llvm/SetFunctionCallConv fnc llvm/CCallConv)
        (binding [*fn* this
                  *llvm-fn* fnc
                  *llvm-locals* newargs]
          (reset! *block* (llvm/AppendBasicBlock fnc (genname "fblk_")))
          (llvm/PositionBuilderAtEnd *builder* @*block*)
          (llvm/BuildRet *builder* (build body) (genname "return_"))
          fnc))))
  GlobalExpression
  (stub-global [this]
    (let [tp (llvm-type type)
          gbl (llvm/AddFunction *module* name tp)]
      (when-let [linkage (:linkage this)]
        (llvm/SetLinkage gbl (llvm/kw->linkage linkage)))
      gbl)))

(defrecord Module [name body]
  Validatable  
  (validate [this]
    (doseq [e body]
      (assure (GlobalExpression? e))
      (valid? e))
    (assure (string? name)))
  
  Expression
  (return-type [this]
    (assert false "Can't get the return type of a module"))
  (build [this]
    (let [error (llvm/new-pointer)
          module (llvm/ModuleCreateWithName name)]
      (binding [*module* module
                *builder* (llvm/CreateBuilder)]
        (doseq [exp body]
          (stub-global exp))
        (doseq [exp body]
          (build exp))
        (Thread/sleep 1000)
        (llvm/VerifyModule module llvm/PrintMessageAction error)
        (llvm/DumpModule module)
        (Thread/sleep 2000)
        (llvm/DisposeMessage (llvm/value-at error))
        module))))

(defrecord If [test then else]
  Validatable
  (validate [this]
    (valid? test)
    (valid? then)
    (valid? else)
    (assure-same-type (return-type then) (return-type else))
    (assure-same-type (return-type test) Int1))
  Expression
  (return-type [this]
    (return-type then))
  (build [this]
    (let [thenblk (llvm/AppendBasicBlock *llvm-fn* (genname "then_"))
          elseblk (llvm/AppendBasicBlock *llvm-fn* (genname "else_"))
          endblk (llvm/AppendBasicBlock *llvm-fn* (genname "end_"))
          cmpval (build test)
          _ (llvm/BuildCondBr *builder* cmpval thenblk elseblk)
          _ (llvm/PositionBuilderAtEnd *builder* thenblk)
          thenval (build then)
          _ (llvm/BuildBr *builder* endblk)
          _ (llvm/PositionBuilderAtEnd *builder* elseblk)
          elseval (build else)
          _ (llvm/BuildBr *builder* endblk)
          _ (llvm/PositionBuilderAtEnd *builder* endblk)
          phi (llvm/BuildPhi *builder*
                             (llvm-type (return-type this))
                             (genname "iflanding_"))]
      (llvm/AddIncoming phi
                        (into-array Pointer [thenval elseval])
                        (into-array Pointer [thenblk elseblk])
                        2)
      phi)))

(defrecord Call [fn args]
  Validatable
  (validate [this]
    (valid? fn)
    (assure (FunctionType? (return-type fn)))
    (doseq [arg args]
      (assure (Expression? arg)))
    (let [cnt (count (:arg-types (return-type fn)))]
      (assure (= (count args) cnt))))
  Expression
  (return-type [this]
    (:ret-type (return-type fn)))
  (build [this]
    (llvm/BuildCall *builder*
                    (build fn)
                    (llvm/map-parr build args)
                    (count args)
                    (genname "call_"))))

(defrecord IAdd [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (assure-same-type (return-type a)
                      (return-type b)))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildAdd *builder* (build a) (build b) (genname "iadd_"))))

(defrecord ISub [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (assure-same-type (return-type a) (return-type b)))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildSub *builder* (build a) (build b) (genname "isub_"))))

(defrecord Local [nm]
  Validatable
  (validate [this]
    (assure (string? nm))
    (println *locals* nm)
    (assure (type? (*locals* nm))))
  Expression
  (return-type [this]
    (*locals* nm))
  (build [this]
    (let [a (*llvm-locals* nm)]
      (assert a (str "Can't find local " nm))
      a)))

(defrecord Loop [itms body]
  Validatable
  (validate [this]
    (doseq [[nm init] itms]
      (assure (string? nm))
      (assure (valid? init)))
    (binding [*locals* (merge *locals*
                              (zipmap (map first itms)
                                      (map (comp return-type second) itms)))
              *recur-point* (map (comp return-type second) itms)]
      (assure (valid? body))))
  Expression
  (return-type [this]
    (binding [*locals* (merge *locals*
                              (zipmap (map first itms)
                                      (map (comp return-type second) itms)))]
      (return-type body)))
  (build [this]
    (binding [*locals* (merge *locals*
                              (zipmap (map first itms)
                                      (map (comp return-type second) itms)))
              *recur-point* (map (comp return-type second) itms)]
      (let [inits (doall (map (fn [itm]
                                (build itm))
                              (map second itms)))
            loopblk (llvm/AppendBasicBlock *llvm-fn* (genname "loop_"))
            endblk (llvm/AppendBasicBlock *llvm-fn* (genname "loopexit_"))
            _ (llvm/BuildBr *builder* loopblk)
            _ (llvm/PositionBuilderAtEnd *builder* loopblk)
            phis (doall (map (fn [itm]
                               (let [phi (llvm/BuildPhi *builder*
                                                    (llvm-type (return-type itm))
                                                    (genname "loopval_"))]
                                 (llvm/AddIncoming phi
                                                   (into-array Pointer [(build itm)])
                                                   (into-array Pointer [@*block*])
                                                   1)
                                 phi))
                             (map second itms)))]
                    
            
       
        (binding [*llvm-locals* (merge *llvm-locals*
                                       (zipmap (map first itms)
                                               phis))
                  *llvm-recur-point* loopblk
                  *llvm-recur-phi* phis]
          
          (let [ret (build body)]
            (llvm/BuildBr *builder* endblk)
            (llvm/PositionBuilderAtEnd *builder* endblk)
            ret))))))

(defrecord Let [nm bind body]
  Validatable
  (validate [this]
    (valid? bind)
    (assure (string? nm))
    (binding [*locals* (assoc *locals*
                         nm (return-type bind))]
      (valid? body)))
  Expression
  (return-type [this]
    (binding [*locals* (assoc *locals*
                         nm (return-type bind))]
      (return-type body)))
  (build [this]
    (binding [*locals* (assoc *locals*
                         nm (return-type bind))
              *llvm-locals* (assoc *llvm-locals*
                              nm (build bind))]
      (build body))))

(defrecord Malloc [type cnt]
  Validatable
  (validate [this]
    (assure (type? type))
    (assure (integer? cnt)))
  Expression
  (return-type [this]
    (->ArrayType type cnt))
  (build [this]
    (llvm/BuildMalloc *builder* (llvm-type (->ArrayType type cnt)) (genname "malloc_"))))

(defrecord ASet [arr idx val]
  Validatable
  (validate [this]
    (assure (vector? idx))
    (doseq [i idx]
      (assure (valid? i)))
    (assure (valid? arr))
    (assure (valid? val))
    (assure (ElementPointer? (return-type arr))))
  Expression
  (return-type [this]
    (return-type arr)))

(defrecord AGet [arr idx]
  Validatable
  (validate [this]
    (assure (valid? arr))
    (assure (vector? idx))
    (doseq [i idx]
      (assure (valid? i)))
    (assure (ElementPointer? (return-type arr))))
  Expression
  (return-type [this]
    (etype (return-type arr))))

(defrecord Recur [items type]
  Validatable
  (validate [this]
    (assure (= (count items) (count *recur-point*)))
    (assure (type? type))
    (dotimes [x (count items)]
      (let [itm (nth items x)
            rp-itm (nth *recur-point* x)]
        (assure-same-type (return-type itm) rp-itm))))
  Expression
  (return-type [this]
    type)
  (build [this]
    (let [d (into-array Pointer
                        (map build items))]
      (llvm/BuildBr *builder* *llvm-recur-point*)
      (reset! *block* (llvm/AppendBasicBlock *llvm-fn* (genname "recur_dummy")))
      (llvm/PositionBuilderAtEnd *builder* @*block*)
      (dotimes [idx (count *llvm-recur-phi*)]
        (llvm/AddIncoming (nth *llvm-recur-phi* idx)
                          (into-array Pointer [(nth *llvm-recur-phi* idx)])
                          (into-array Pointer [*llvm-recur-point*])
                          1))
      (build 0))))

(defrecord Free [val]
  Validatable
  (validate [this]
    (ElementPointer? (return-type val)))
  Expression
  (return-type [this]
    Int32))

(defrecord Do [body]
  Validatable
  (validate [this]
    (doseq [exp body]
      (assure (Expression? exp))
      (assure (valid? exp))))
  Expression
  (return-type [this]
    (return-type (last body)))
  (build [this]
    (doseq [exp (butlast body)]
      (build exp))
    (build (last body))))

(defrecord Global [name type]
  Validatable
  (validate [this]
    (assure (string? name)))
  Expression
  (return-type [this]
    type)
  (build [this]
    (llvm/GetNamedFunction *module* name)))

(defn Module? [mod]
  (instance? Module mod))

(defn compile-module [module]
  {:pre [(Module? module)]}
  (build module))


(extend-type java.lang.Long
  Validatable
  (validate [this]
    true)
  Expression
  (return-type [this]
    Int32)
  (build [this]
    (llvm/ConstInt (llvm-type (return-type this))
                   this
                   true)))

(extend-type java.lang.Double
  Validatable
  (validate [this]
    true)
  Expression
  (return-type [this]
    Double)
  #_(build [this]
    (llvm/ConstFloat this)))


(defn engine [module]
  (let [provider (llvm/CreateModuleProviderForExistingModule module)
          error (llvm/new-pointer)
          engine (llvm/new-pointer)]
      (assert provider)
      (when-not (= (llvm/CreateJITCompiler  engine provider 2 error) 0)
        (assert false (.getString error 0 false)))
      (llvm/DisposeMessage (llvm/value-at error))
      (assert (llvm/value-at engine))      
      engine))

(defn java-to-llvm-arg [x]
  (cond
   (integer? x) (llvm/CreateGenericValueOfInt (llvm/Int32Type) x 0)
   :else (assert false "Can't convert value")))

(defn get-fn [engine module name]
  (let [f (llvm/GetNamedFunction module name)]
    (fn [& args]
      (let [p (into-array Pointer (map java-to-llvm-arg args))
            ex_res (llvm/RunFunction (llvm/value-at engine) f (count p) p)
            ires (llvm/GenericValueToInt ex_res 0)]
        (doseq [x p]
          (llvm/DisposeGenericValue x))
        (llvm/DisposeGenericValue ex_res)
        ires))))