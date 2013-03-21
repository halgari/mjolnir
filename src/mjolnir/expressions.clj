(ns mjolnir.expressions
  (:require [mjolnir.types :refer :all]
            [mjolnir.ssa :as ssa]
            [mjolnir.llvmc :as llvm]
            [mjolnir.config :refer :all]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [mjolnir.targets.target :as target])
  (:import [com.sun.jna Native Pointer]))

(def genname (comp name gensym))

(defprotocol Expression
  (return-type [this])
  (build [this]))


(defn Expression? [this]
  (extends? Expression (type this)))

(defprotocol SSAWriter
  (write-ssa [this plan]))


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

(defrecord ConstVector [vals]
  Validatable
  (validate [this]
    )
  Expression
  (return-type [this]
    (->VectorType (return-type (first vals)) (count vals)))
  (build [this]
    (llvm/ConstVector (into-array Pointer
                                  (map build vals))
                      (count vals))))

(defn- const-data [val]
  (cond
   (integer? val) {:const/int-value val}))

(defrecord Const [type val]
  Validatable
  (validate [this]
    (assure (type? type)))
  Expression
  (return-type [this]
    type)
  (build [this]
    (encode-const type val))
  #_SSAWriter
  #_(write-ssa [this plan]
    (let [with-type (ssa/add-to-plan plan type)
          with-const (ssa/add-instruction with-type
                                          :inst.type/const
                                          this
                                          (merge
                                           (const-data val)
                                           {:const/type (ssa/plan-id with-type type)}))]
      [with-const (ssa/plan-id with-const this)])))

(defrecord BitCast [ptr tp]
  Validatable
  (validate [this]
    (assure (Expression? ptr))
    (assure (type? tp)))
  Expression
  (return-type [this]
    tp)
  (build [this]
    (assert (not (keyword ptr)) ptr)
    (llvm/BuildBitCast *builder* (build ptr) (llvm-type tp) (genname "bitcast_"))))

(defrecord Trunc [a tp]
  Validatable
  (validate [this]
    (assure (Expression? a))
    (assure (type? tp)))
  Expression
  (return-type [this]
    tp)
  (build [this]
    (llvm/BuildTrunc *builder* (build a) (llvm-type tp) (genname "trunk_"))))

(defrecord ZExt [a tp]
  Validatable
  (validate [this]
    (assure (Expression? a))
    (assure (type? tp)))
  Expression
  (return-type [this]
    tp)
  (build [this]
    (llvm/BuildZExt *builder* (build a) (llvm-type tp) (genname "zext_"))))

(defrecord Add [a b]
  Validatable
  (validate [this]
    (validate-all a b)
    (assure-same-type a b))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildAdd *builder* (build a) (build b) (genname "add_"))))


(defrecord IMul [a b]
  Validatable
  (validate [this]
    (validate-all a b)
    (assure-same-type a b))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildMul *builder* (build a) (build b) (genname "imul_"))))

(defrecord FMul [a b]
  Validatable
  (validate [this]
    (validate-all a b)
    (assure-same-type a b))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildFMul *builder* (build a) (build b) (genname "fmul_"))))

(defrecord FDiv [a b]
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
    (llvm/BuildFDiv *builder* (build a) (build b) (genname "fdiv_"))))


(defrecord And [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (assure-same-type (return-type a) Int1)
    (assure-same-type (return-type b) Int1))
  Expression
  (return-type [this]
    Int1)
  (build [this]
    (llvm/BuildAnd *builder*
                   (build a)
                   (build b)
                   (genname "and_"))))

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

(def cmp-maps
  {:int {:= llvm/LLVMIntEQ
         :!= llvm/LLVMIntNE
         :> llvm/LLVMIntSGT
         :< llvm/LLVMIntSLT
         :<= llvm/LLVMIntSLE
         :>= llvm/LLVMIntSGE}
   :float {:= llvm/LLVMRealOEQ
         :!= llvm/LLVMRealONE
         :> llvm/LLVMRealOGT
         :< llvm/LLVMRealOLT
         :<= llvm/LLVMRealOLE
         :>= llvm/LLVMRealOGE}})

(defrecord Cmp [pred a b]
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
    (let [[tp f]
          (cond
           (integer-type? (return-type a)) [:int llvm/BuildICmp]
           (float-type? (return-type a)) [:float llvm/BuildFCmp]
           (vector-type? (return-type a)) [:float llvm/BuildFCmp])]
      (assert (pred (tp cmp-maps)) "Invalid predicate symbol")
      (f *builder* (pred (tp cmp-maps)) (build a) (build b) (genname "cmp_")))))

(defrecord Not [a]
  Validatable
  (validate [this]
    (assure (valid? a))
    (assure-same-type (return-type a) Int1))
  Expression
  (return-type [this]
    Int1)
  (build [this]
    (llvm/BuildNot *builder* (build a) "not_")))

(defprotocol IFunctionExpression
  (argument [this idx] "Get an expression for the given argument"))

(defprotocol NamedExpression
  (get-name [this]))

(defrecord Argument [idx tp]
  Validatable
  (validate [this]
    (assure (argument *fn* idx)))
  Expression
  (return-type [this]
    tp)
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

(defn full-name [n]
  (cond (string? n) n
        (and (keyword? n) (namespace n)) (str (namespace n) "/" (name n))
        (keyword? n) (name n)
        :else (assert false (str "Can't get name of " (pr-str n)))))

(defrecord GetGlobal [name tp]
  Validatable
  (validate [this]
    (assure (or (string? name)
                (keyword? name)))
    (assure (type? tp)))
  Expression
  (return-type [this]
    tp)
  (build [this]
    (let [val (if (FunctionType? tp)
                 (llvm/GetNamedFunction *module* (full-name name))
                 (llvm/GetNamedGlobal *module* (full-name name)))]
      (assert val (str "Global not found " (full-name name)))
      val)))

(defrecord SizeOf [tp]
  Validatable
  (validate [this]
    (assure (type? tp)))
  Expression
  (return-type [this]
    Int64)
  (build [this]
    (llvm/SizeOf (llvm-type tp))))




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
      (when body
        (assure (Expression? body))
        (valid? body)
        (when (not= (:ret-type type) VoidT)
          (assure-same-type (return-type body) (:ret-type type))))
      (assure (= (count (:arg-types type)) (count arg-names)))
      
      
      (when-let [linkage (:linkage this)]
        (assert (llvm/kw->linkage linkage) (str "No Linkage " linkage)))))
  Expression
  (return-type [this]
    type)
  (build [this]
    (when body
      (let [fnc (llvm/GetNamedFunction *module* name)
            newargs (into {} (map (fn [s idx]
                                    [s (llvm/GetParam fnc idx )])
                                  arg-names
                                  (range (count arg-names))))]
        (binding [*fn* this
                  *llvm-fn* fnc
                  *llvm-locals* newargs]
          (reset! *block* (llvm/AppendBasicBlock fnc (genname "fblk_")))
          (llvm/PositionBuilderAtEnd *builder* @*block*)
          (if (= (:ret-type type) VoidT)
            (do
              (build body)
              (llvm/BuildRetVoid *builder*))
            (llvm/BuildRet *builder* (build body) (genname "return_")))
          fnc))))
  GlobalExpression
  (stub-global [this]
    (let [tp (llvm-type type)
          gbl (llvm/AddFunction *module* name tp)]
      (llvm/SetFunctionCallConv gbl (target/get-calling-conv *target*
                                                             (= :extern
                                                                (:linkage this))))
      (llvm/SetLinkage gbl (llvm/kw->linkage :extern))
      gbl))
  #_ssa/IToPlan
  #_(ssa/-add-to-plan [this plan]
    (gen-plan
      [args (assert-all (map (fn [idx name]
                               (let [a {:argument/name name
                                        :argument/idx idx}]
                                 [a a]))
                             (range)
                             arg-names))
       head-node (assert-seq args)
       fn-id (assert-entity {:node/type :node.type/fn
                              :fn/type (ssa/plan-id with-args type)
                              :fn/name name
                             :fn/argument-names head-node})
       block-id (add-entry-block fn-id)
       body-id (write-ssa body block-id)]
      [fn-id])
    (let [args (map (fn [idx name]
                      {:argument/name name
                       :argument/idx idx})
                    (range)
                    arg-names)
          with-nodes (reduce
                      (fn [acc e]
                        (ssa/assert-entity acc e e))
                      (ssa/add-to-plan plan type)
                      args)
          [with-args names-id] (ssa/assert-seq with-nodes
                                               (map (partial ssa/plan-id with-nodes)
                                                    args))
          with-this (ssa/assert-entity
                     with-args
                     {:node/type :node.type/fn
                      :fn/type (ssa/plan-id with-args type)
                      :fn/name name
                      :fn/argument-names names-id}
                     this)
          this-id (ssa/plan-id with-this this)
          [with-block block-id] (ssa/add-entry-block with-this this-id)]
      (binding [*fn* this-id]
        (let [[with-body body-id] (write-ssa body (ssa/set-block with-block block-id))]
          (ssa/add-instruction with-body :inst.type/return-value {:return this} {:inst/return-value body-id}))))))

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
    (let [_ (valid? this)
          error (llvm/new-pointer)
          module (llvm/ModuleCreateWithName name)]
      (binding [*module* module
                *builder* (llvm/CreateBuilder)]
        (doseq [exp body]
          (stub-global exp))
        (doseq [exp body]
          (build exp))
        #_(Thread/sleep 1000)
        (llvm/VerifyModule module llvm/PrintMessageAction error)
        #_(llvm/DumpModule module)
        #_(Thread/sleep 1000)
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
          _ (reset! *block* thenblk)
          thenval (build then)
          post-thenblk @*block*
          _ (when-not (= thenval :terminated)
              (llvm/BuildBr *builder* endblk))
          _ (llvm/PositionBuilderAtEnd *builder* elseblk)
          _ (reset! *block* elseblk)
          elseval (build else)
          post-elseblk @*block*
          _ (when-not (= elseval :terminated)
              (llvm/BuildBr *builder* endblk))
          _ (llvm/PositionBuilderAtEnd *builder* endblk)
          _ (reset! *block* endblk)
          phi (llvm/BuildPhi *builder*
                             (llvm-type (return-type this))
                             (genname "iflanding_"))]
      (when-not (= thenval :terminated)
        (llvm/AddIncoming phi
                          (into-array Pointer [thenval])
                          (into-array Pointer [post-thenblk])
                          1))
      (when-not (= elseval :terminated)
        (llvm/AddIncoming phi
                          (into-array Pointer [elseval])
                          (into-array Pointer [post-elseblk])
                          1))      
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

(defrecord CallPointer [fn args]
  Validatable
  (validate [this]
    (valid? fn)
    (assure (FunctionType? (etype (return-type fn))))
    (doseq [arg args]
      (assure (Expression? arg)))
    (let [cnt (count (:arg-types (etype (return-type fn))))]
      (assure (= (count args) cnt))))
  Expression
  (return-type [this]
    (:ret-type (etype (return-type fn))))
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

(defrecord FAdd [a b]
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
    (llvm/BuildFAdd *builder* (build a) (build b) (genname "iadd_"))))

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

(defrecord FSub [a b]
  Validatable
  (validate [this]
    (valid? a)
    (valid? b)
    (assure-same-type (return-type a) (return-type b)))
  Expression
  (return-type [this]
    (return-type a))
  (build [this]
    (llvm/BuildFSub *builder* (build a) (build b) (genname "fsub_"))))

(defrecord Local [nm]
  Validatable
  (validate [this]
    (assure (string? nm))
    (assert (*locals* nm) (str "Couldn't find " nm " in locals: " *locals*))
    (assure (type? (*locals* nm))))
  Expression
  (return-type [this]
    (*locals* nm))
  (build [this]
    (let [a (*llvm-locals* nm)]
      (assert a (str "Can't find local " nm))
      a)))


(defrecord +Op [exprs]
  Validatable
  (validate [this]
    (doseq [exp exprs]
      (assure (valid? exp)))
    (assert (apply = (map return-type exprs))
            "Every Expression in a + must return the same type"))
  Expression
  (return-type [this]
    (return-type (first exprs)))
  (build [this]
    (-> (reduce (fn [a x]
                  (cond
                    (integer-type? (return-type a)) (->IAdd a x)
                    (float-type? (return-type a)) (->FAdd a x)
                    (vector-type? (return-type a)) (->FAdd a x)))
                (first exprs)
                (next exprs))
        build)))

(defrecord -Op [exprs]
  Validatable
  (validate [this]
    (doseq [exp exprs]
      (assure (valid? exp)))
    (assert (apply = (map return-type exprs))
            "Every Expression in a - must return the same type"))
  Expression
  (return-type [this]
    (return-type (first exprs)))
  (build [this]
    (-> (reduce (fn [a x]
                  (cond
                    (integer-type? (return-type a)) (->ISub a x)
                    (float-type? (return-type a)) (->FSub a x)
                    (vector-type? (return-type a)) (->FSub a x)))
                (first exprs)
                (next exprs))
        build)))


(defrecord *Op [exprs]
  Validatable
  (validate [this]
    (doseq [exp exprs]
      (assure (valid? exp)))
    (assert (apply = (map return-type exprs))
            "Every Expression in a * must return the same type"))
  Expression
  (return-type [this]
    (return-type (first exprs)))
  (build [this]
    (-> (reduce (fn [a x]
                  (cond
                    (integer-type? (return-type a)) (->IMul a x)
                    (float-type? (return-type a)) (->FMul a x)
                    (vector-type? (return-type a)) (->FMul a x)))
                (first exprs)
                (next exprs))
        build)))


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
            fromblk @*block*
            _ (reset! *block* loopblk)
            phis (doall (map (fn [[built exp]]
                               (let [phi (llvm/BuildPhi *builder*
                                                    (llvm-type (return-type exp))
                                                    (genname "loopval_"))]
                                 (llvm/AddIncoming phi
                                                   (into-array Pointer [built])
                                                   (into-array Pointer [fromblk])
                                                   1)
                                 phi))
                             (map vector inits (map second itms))))]
                    
            
       
        (binding [*llvm-locals* (merge *llvm-locals*
                                       (zipmap (map first itms)
                                               phis))
                  *llvm-recur-point* loopblk
                  *llvm-recur-phi* phis]
          
          (let [ret (build body)]
            (llvm/BuildBr *builder* endblk)
            (reset! *block* endblk)
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

#_(defrecord Alloc [type cnt]
  Validatable
  (validate [this]
    (assure (type? type))
    (assure (integer? cnt)))
  Expression
  (return-type [this]
    (->ArrayType type cnt))
  (build [this]
    (llvm/BuildAlloc *builder* (llvm-type (->ArrayType type cnt)) (genname "malloc_"))))


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
    (return-type arr))
  (build [this]
    (let [a (build arr)
          casted (llvm/BuildBitCast *builder*
                                    a
                                    (-> this
                                        return-type
                                        etype
                                        ->PointerType
                                        llvm-type)
                                    (genname "casted_"))
          gep (llvm/BuildGEP *builder*
                            casted
                            (into-array Pointer
                                        (map build idx))
                            (count idx)
                            (genname "gep_"))]
      (llvm/BuildStore *builder* (build val) gep)
      a)))

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
    (etype (return-type arr)))
  (build [this]
    (let [casted (llvm/BuildBitCast *builder*
                                    (build arr)
                                    (-> this
                                        return-type
                                        ->PointerType
                                        llvm-type)
                                    (genname "casted_"))
          gep (llvm/BuildGEP *builder*
                             casted
                            (into-array Pointer
                                        (map build idx))
                            (count idx)
                            (genname "gep_"))]
      (llvm/BuildLoad *builder*
                      gep
                      (genname "load_")))))

(defrecord Set [ptr member val]
  Validatable
  (validate [this]
    (assure (valid? ptr))
    (assure (keyword? member))
    (assure (valid? val))
    (assure (pointer-type? (return-type ptr)))
    (let [etp (etype (return-type ptr))
          mt (member-idx etp member)]
      (assert (identity mt) (vector (flatten-struct (return-type ptr))
                                    (return-type ptr)))
      (assure-same-type (first (nth (flatten-struct etp) mt))
                        (return-type val))))
  
  Expression
  (return-type [this]
    (return-type ptr))
  (build [this]
    (let [etp (etype (return-type ptr))
          idx (member-idx etp
                          member)
          _ (assert idx (pr-str "Idx error, did you validate first? " ptr " " member))
          bptr (build ptr)
          cptr (build (->BitCast ptr  (->PointerType etp)))
          gep (llvm/BuildStructGEP *builder* cptr idx (genname "set_"))]
      (llvm/BuildStore *builder* (build val) gep)
      bptr)))


(defrecord Store [ptr val]
  Validatable
  (validate [this]
    (assure (valid? ptr))
    (assure (valid? val)))
  Expression
  (return-type [this]
    (return-type ptr))
  (build [this]
    (build (->ASet ptr [0] val))))


(defrecord Get [ptr member]
  Validatable
  (validate [this]
    (assure (valid? ptr))
    (assure (keyword? member))
    (assure (pointer-type? (return-type ptr)))
    (assure (member-idx (etype (return-type ptr))
                        member)))
  Expression
  (return-type [this]
    (let [idx (member-idx (etype (return-type ptr))
                          member)]
      (-> ptr return-type etype flatten-struct (nth idx) first)))
  (build [this]
    (let [etp (etype (return-type ptr))
          idx (member-idx etp
                          member)
          _ (assert idx "Idx error, did you validate first?")
          cptr (build (->BitCast ptr (->PointerType etp)))
          gep (llvm/BuildStructGEP *builder* cptr idx (genname "get_"))] 
      (llvm/BuildLoad *builder* gep (genname "load_")))))


(defrecord EGet [vec member]
  Validatable
  (validate [this]
    (assure (vector-type? (return-type vec)))
    (assure (integer? member))
    (assure (< member (:length (return-type vec)))))
  Expression
  (return-type [this]
    (etype (return-type vec)))
  (build [this]
    (llvm/BuildExtractElement *builder*
                              (build vec)
                              (encode-const Int32 member)
                              (genname "eget_"))))

(defrecord ESet [vec member val]
  Validatable
  (validate [this]
    (assure (vector-type? (return-type vec)))
    (assure (integer? member))
    (assure-same-type (etype (return-type vec))
                      (return-type val))
    (assure (< member (:length (return-type vec)))))
  Expression
  (return-type [this]
    (return-type vec))
  (build [this]
    (llvm/BuildInsertElement *builder*
                             (build vec)
                             (build val)
                             (encode-const Int32 member)
                             (genname "eget_"))))

(defrecord New [tp vals]
  Validatable
  (validate [this]
    (assure (StructType? tp))
    (assure (= (count (flatten-struct tp)) (count vals)))
    (doall (map (fn [[tp name] o]
                  (assure-same-type tp (return-type o)))
                (flatten-struct tp)
                vals)))
  Expression
  (return-type [this]
    (->PointerType tp))
  (build [this]
    (let [malloc (llvm/BuildMalloc *builder* (llvm-type tp) (genname "new_"))
          members (flatten-struct tp)]
      (doseq [idx (range (count vals))]
        (let [gep (llvm/BuildStructGEP *builder*
                                       malloc
                                       idx
                                       (genname "gep_"))]
          (llvm/BuildStore *builder* (build (nth vals idx)) gep)))
      malloc)))

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
    (let [d (mapv build items)]
      (llvm/BuildBr *builder* *llvm-recur-point*)
      (dotimes [idx (count *llvm-recur-phi*)]
        (llvm/AddIncoming (nth *llvm-recur-phi* idx)
                          (into-array Pointer [(nth d idx)])
                          (into-array Pointer [@*block*])
                          1))
      :terminated)))

(defrecord Free [val]
  Validatable
  (validate [this]
    (ElementPointer? (return-type val)))
  Expression
  (return-type [this]
    Int32)
  (build [this]
    (llvm/BuildFree *builder* (build val))
    (build 0)))

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

(defrecord Global [name val type]
  Validatable
  (validate [this]
    (assure (string? name))
    (assure (type? type)))
  Expression
  (return-type [this]
    type)
  (build [this]
    (when (not (= val :extern))
      (let [gbl (llvm/GetNamedGlobal *module* name)]
        (assert gbl (str "Can't find Global " (pr-str this)))
        (llvm/SetInitializer
         gbl
         (encode-const type val))
        gbl)))
  GlobalExpression
  (stub-global [this]
    (println name)
    (llvm/AddGlobalInAddressSpace *module*
                    (llvm-type type)
                    name
                    (target/default-address-space *target*))))

(defrecord FPToSI [val tp]
  Validatable
  (validate [this])
  Expression
  (return-type [this]
    tp)
  (build [this]
    (llvm/BuildFPToSI *builder* (build val) (llvm-type tp) (genname "toui_"))))

(defrecord SIToFP [val tp]
  Validatable
  (validate [this])
  Expression
  (return-type [this]
    tp)
  (build [this]
    (llvm/BuildSIToFP *builder* (build val) (llvm-type tp) (genname "toui_"))))

(defn kw->Global [module kw]
  (assert module "No Module Given")
  (assert kw "No KW name given")
  (let [f (->> module
               :body
               (filter #(= (:name %)
                           (name kw)))
               first)
        
        gg (->GetGlobal (name kw)
                        (:type f))]
    (assert f (str "Global not found " kw))
    f))

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
    (assert *int-type* "No type set for ints")
    *int-type*)
  (build [this]
    (encode-const *int-type* this))
  SSAWriter
  (write-ssa [this plan]
    (write-ssa (->Const *int-type* this) plan)))

(extend-type java.lang.Double
  Validatable
  (validate [this]
    true)
  Expression
  (return-type [this]
    (assert *float-type* "No type set for floats")
    *float-type*)
  (build [this]
    (encode-const *float-type* this))
  #_ (comment ssa/IToDatoms
              (ssa/-to-datoms [this conn]
                              (ssa/transact-new
                               conn
                               {:node/type :type/const
                                :node/return-type (-> (ssa/-to-datoms
                                                       *float-type*
                                                       conn)
                                                      :db/id)}))))




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
   (integer? x) (llvm/CreateGenericValueOfInt (llvm/Int64Type) x 0)
   :else (assert false "Can't convert value")))

(defn get-fn [engine module name]
  (let [f (llvm/GetNamedFunction module name)
        ftype (*)]
    (assert f "Can't find function")
    (fn [& args]
      (println "Running..." args f)
      (let [p (into-array Pointer (map java-to-llvm-arg args))
            ex_res (llvm/RunFunction (llvm/value-at engine) f (count p) p)
            ires (llvm/GenericValueToInt ex_res 0)]
        (doseq [x p]
          (llvm/DisposeGenericValue x))
        (llvm/DisposeGenericValue ex_res)
        ires))))





;; compilation

(defn temp-file [prefix ext]
  (let [file (java.io.File/createTempFile prefix ext)]
    (.deleteOnExit file)
    (.getCanonicalPath file)))

(defn dump-module-to-temp-file [module]
  (let [file (temp-file "mod_dump" ".bc")]
    (llvm/WriteBitcodeToFile module file)
    file))


(defn verify [module]
  (llvm/VerifyModule module)
  module)


(defn optimize [module]
  (let [pass (llvm/CreatePassManager)]
    (llvm/AddDefaultPasses pass)
    (llvm/RunPassManager pass module)
    (llvm/DisposePassManager pass)
    #_(llvm/DumpModule module)
    module))

(defn dump [module]
  (llvm/DumpModule module))

(defn write-object-file [module march] 
  (let [file (dump-module-to-temp-file module)
        ofile (temp-file "o_dump" ".o")
        cmds ["/usr/local/bin/llc" "-filetype=obj" "-o" ofile file]
        cmds (if march (concat cmds ["--march" march]) cmds)
        {:keys [out err exit] :as mp} (apply shell/sh cmds)]
    (apply shell/sh ["/usr/local/bin/llc" "-filetype=asm" "-o" "foo.s" file])
    (println cmds)
    (assert (= exit 0) err)
    
    ofile))

(defn interpret-opt [op]
  (cond (vector? op)
        (let [res (apply shell/sh op)]
          (assert (= 0 (:exit res)) (:err res))
          (string/split (string/trim (:out res)) #"[ \n]"))
        :else
        [op]))

(defn link-object-file [module filename march & opts]
  (let [tmp (write-object-file module march)
        opts (mapcat interpret-opt opts)
        cmds (concat ["gcc" tmp]
                                    opts
                                    ["-o" filename "--shared"])
        _ (println cmds)
        res (apply shell/sh cmds)]
    (assert (= 0 (:exit res)) res)
    (:out res)))

(defn link-exe [obj out]
  (let [cmds (concat ["gcc" obj "-o" out "-lc"])
        _ (println cmds)
        res (apply shell/sh cmds)]
    (assert (= 0 (:exit res)) res)
    (:out res)))



(defn compile-as-exe [mod opts]
  (let [ofile (write-object-file mod "x86-64")
        exe-file (or (:out opts) (temp-file "exe_gen" "out"))
        out (link-exe ofile exe-file)]
    exe-file))

(defn run-exe [file & args]
     (apply shell/sh file args))

;;;;;;;;;; Target Machine ;;;;;;;;;;;;;;

