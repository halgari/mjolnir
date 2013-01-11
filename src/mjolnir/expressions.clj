(ns mjolnir.expressions
  (:require [mjolnir.types :refer :all])
  (:require [mjolnir.llvmc :as llvm]))

(def ^:dynamic *builder*)
(def ^:dynamic *module*)
(def ^:dynamic *fn*)
(def ^:dynamic *llvm-fn*)
(def ^:dynamic *locals*)
(def ^:dynamic *block*)

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
    (return-type (argument *fn* idx))))

(defrecord FnArgument [arg-name arg-idx arg-type]
    Validatable
    (validate [this]
      (assure (string? arg-name))
      (assure (type? arg-type))
      (assure (integer? arg-idx)))
    Expression
    (return-type [this]
      arg-type)
    (build [this]))




(defprotocol GlobalExpression
  (stub-global [this]))

(defn GlobalExpression? [exp]
  (extends? GlobalExpression (type exp)))

(defrecord Fn [name type arg-names body]
  Validatable
  (validate [this]
    (assure (string? name))
    (assure-type type)
    (assure (FunctionType? type))
    (assure (every? string? arg-names))
    (assure (Expression? body))
    (assure (= (count (:arg-types type)) (count arg-names)))
    (valid? body)
    (assure (= (return-type body) (:ret-type type)))
    (when-let [linkage (:linkage this)]
      (assure (llvm/kw->linkage linkage))))
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
        (binding [*llvm-fn* fnc
                  *locals* newargs
                  *block* (llvm/AppendBasicBlock fnc (genname "fblk_"))]
          (llvm/PositionBuilderAtEnd *builder* *block*)
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
      (assure (string? name))))
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
        (llvm/VerifyModule module llvm/AbortProcessAction error)
        (llvm/DisposeMessage (llvm/value-at error))
        module))))

(defrecord If [test then else]
  Validatable
  (validate [this]
    (valid? test)
    (valid? then)
    (valid? else)
    (assure-same-type (return-type then) (return-type else))
    (assure-same-type (return-type test) (->IntegerType 1)))
  Expression
  (return-type [this]
    (:ret-type (return-type then))))

(defrecord Call [fn args]
  Validatable
  (validate [this]
    (valid? fn)
    (assure (FunctionType? (return-type fn)))
    (doseq [arg args]
      (assure (Expression? arg)))
    (let [cnt (count (:arg-names fn))]
      (assure (= (count args) cnt))))
  Expression
  (return-type [this]
    (return-type fn))
  (build [this]
    (llvm/BuildCall *builder*
                    (build fn)
                    (llvm/map-parr build args)
                    (count args)
                    (genname "call_"))))


(defn Module? [mod]
  (instance? Module mod))

(defn compile-module [module]
  {:pre [(Module? module)]}
  (build module))


