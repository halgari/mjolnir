(ns mjolnir.expressions)

(def ^:dynamic *builder*)
(def ^:dynamic *fn*)

(defmacro ensure [& body]
  (let [itms (partition 2 body)]))

(defprotocol Expression
  (validate [this state])
  (return-type [this])
  (build [this]))


(defn expression? [this]
  (extends? Expression (type this)))


(defrecord ConstInteger [value type]
  Expression
  (validate [this]
    (ensure (integer? value))
    (ensure (type? type)))
  (return-type [this]
    type)
  (build [this builder]
    (LLVMConstInt (llvm-type type) value true)))



(defrecord Add [a b]
  Expression
  (validate [this]
    (validate-all a b)
    (ensure (same-type a b)))
  (return-type [this]
    (return-type a))
  (build [this]
    (LLVMBuildAdd *builder* (build a) (build b))))


(defprotocol IFunctionExpression
  (argument [this idx] "Get an expression for the given argument"))

(defprotocol NamedExpression
  (get-name [this]))

(defrecord Argument [idx]
  Expression
  (validate [this]
    (ensure (argument *fn* idx)))
  (return-type [this]
    (return-type (argument *fn* idx))))



;; Atoms can be expressions if their contents are correct 
(extend-type clojure.lang.Atom
  Expression
  (validate [this]
    (validate-all @this))
  (return-type [this]
    (return-type @this))
  (build [this]
    (build @this)))

