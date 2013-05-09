(ns mjolnir.expressions
  (:require [mjolnir.types :refer :all]
            [mjolnir.ssa :as ssa :refer :all]
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
  (write-ssa [this]))



(defn- const-data [val]
  (cond
   (integer? val) {:const/int-value val}
   (float? val) {:const/float-value val}))

(defrecord Const [type val]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [tp (add-to-plan type)
      const (add-instruction :inst.type/const
                             (merge
                              (const-data val)
                              {:const/type tp})
                             key)]
     const)))

(defrecord Cast [tp expr]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [tp-id (add-to-plan tp)
      expr-id (write-ssa expr)
      casted (add-instruction :inst.type/cast
                              {:inst.arg/arg0 expr-id
                               :node/return-type tp-id})]
     casted)))

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
  SSAWriter
  (write-ssa [this]
    (let [pred (keyword "inst.cmp.pred" (name pred))]
      (gen-plan
       [tp (add-to-plan Int1)
        lh (write-ssa a)
        rh (write-ssa b)
        nd (add-instruction :inst.type/cmp
                            {:node/return-type tp
                             :inst.arg/arg0 lh
                             :inst.arg/arg1 rh
                             :inst.cmp/pred pred}
                            nil)]
       nd))))

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


(defrecord Arg [idx]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [this-id (add-instruction :inst.type/arg
                               {:inst.arg/idx idx}
                               this)]
     this-id)))

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

(defrecord Gbl [name]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [gbl (add-instruction :inst.type/gbl
                           {:inst.gbl/name name}
                           this)]
     gbl)))

(defrecord SizeOf [tp]
  Validatable
  (validate [this]
    (assure (type? tp)))
  Expression
  (return-type [this]
    Int64)
  (build [this]
    (llvm/SizeOf (llvm-type tp))))

(def binop-maps
  {:+ :inst.binop.type/add
   :- :inst.binop.type/sub
   :* :inst.binop.type/mul
   :div :inst.binop.type/div
   :and :inst.binop.type/and
   :or :inst.binop.type/or})



(defrecord Binop [op lh rh]
  SSAWriter
  (write-ssa [this]
    (assert (binop-maps op) (str "Invalid binop"))
    (gen-plan
     [lh-id (write-ssa lh)
      rh-id (write-ssa rh)
      inst (add-instruction :inst.type/binop
                            {:inst.arg/arg0 lh-id
                             :inst.arg/arg1 rh-id
                             :inst.binop/type (binop-maps op)}
                            this)]
     inst)))


(defprotocol GlobalExpression
  (stub-global [this]))

(defn GlobalExpression? [exp]
  (extends? GlobalExpression (type exp)))

(defrecord Fn [name type arg-names body]
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [type-id (add-to-plan type) 
      args (assert-all (map (fn [idx name]
                              (let [a {:argument/name name
                                       :argument/idx idx}]
                                [a a]))
                            (range)
                            arg-names))
      head-node (assert-seq args)
      fn-id (assert-entity (merge {:node/type :node.type/fn
                                   :fn/type type-id
                                   :fn/name name}
                                  (when head-node
                                    {:fn/argument-names head-node}))
                           this)
      _ (assoc-in-plan [:state :fn] fn-id)
      _ (if body
          (gen-plan
           [block-id (add-entry-block fn-id)
            body-id (write-ssa body)
            ret-id (terminate-block :inst.type/return-val body-id)]
           ret-id)
          (mark-extern-fn fn-id))]
     [fn-id])))

(defrecord Module [body]
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [ids (add-all (map add-to-plan body))]
     ids)))

(defrecord If [test then else]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [fnc (get-in-plan [:state :fn])

      test-id (write-ssa test)
      test-block (get-block)
      
      pre-then-block (add-block fnc "then")
      _ (set-block pre-then-block)
      then-val (write-ssa then)
      post-then-block (get-block)
      then-terminated? (terminated? post-then-block)
      
      pre-else-block (add-block fnc "else")
      _ (set-block pre-else-block)
      else-val (write-ssa else)
      post-else-block (get-block)
      else-terminated? (terminated? post-else-block)

      merge-block (add-block fnc "merge")
      _ (set-block merge-block)
      phi-val (add-phi)

      _ (set-block test-block)
      br-id (terminate-block :inst.type/br test-id pre-then-block pre-else-block)

      _ (if then-terminated?
          (no-op)
          (gen-plan
           [_ (set-block post-then-block)
            _ (terminate-block :inst.type/jmp merge-block)
            _ (add-to-phi phi-val post-then-block then-val)]
           nil))

      _ (if else-terminated?
          (no-op)
          (gen-plan
           [_ (set-block post-else-block)
            _ (terminate-block :inst.type/jmp merge-block)
            _ (add-to-phi phi-val post-else-block else-val)]
           nil))      

      _ (set-block merge-block)]
     phi-val)))

(defrecord Call [fnc args]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [fnc (write-ssa fnc)
      lst (add-all (map write-ssa args))
      call-id (add-instruction :inst.type/call
                               (reduce
                                (fn [acc [idx id]]
                                  (assoc acc (idx->arg idx) id))
                                {:inst.call/fn fnc}
                                (map vector
                                     (range)
                                     lst))
                               this)]
     call-id)))

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

(defrecord Local [nm]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [locals (get-binding :locals)
      p (get-in-plan [:bindings])]
     (let [a (locals nm)]
       (assert a (str "Can't find local " nm " in " locals))
       a))))


(defrecord Loop [itms body]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [fnc (get-in-plan [:state :fn])
      itm-ids (add-all (map (comp write-ssa second) itms))
      recur-pnt (add-block fnc "body")
      _ (terminate-block :inst.type/jmp recur-pnt)
      prev-block (get-block)
      _ (set-block recur-pnt)
      phis (add-all (map (fn [x] (add-phi))
                         (range (count itms))))
      _ (add-all (map (fn [phi-node val]
                           (add-to-phi phi-node prev-block val))
                         phis
                         itm-ids))
      _ (apply push-alter-binding :locals assoc (mapcat (fn [[nm _] val]
                                                          [nm val])
                                                        itms
                                                        phis))
      _ (push-binding :recur recur-pnt)
      _ (push-binding :recur-phis phis)
      return-val (write-ssa body)
      _ (pop-binding :recur-phis)
      _ (pop-binding :recur)
      _ (pop-binding :locals)
      end-block (add-block fnc "end")
      _ (terminate-block :inst.type/jmp end-block)
      _ (set-block end-block)]
     return-val)))

(defrecord Let [nm bind body]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [bind-id (write-ssa bind)
      _ (push-alter-binding :locals assoc nm bind-id)
      val (write-ssa body)
      _ (pop-binding :locals)]
     val)))

(defrecord Malloc [type]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [tp-id (add-to-plan type)
      inst-id (add-instruction :inst.type/malloc
                               {:inst.malloc/type tp-id
                                :node/return-type tp-id})]
     inst-id)))

(defrecord Free [itm]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [itm (write-ssa itm)
      void (add-to-plan VoidT)
      inst-id (add-instruction :inst.type/free
                               {:inst.arg/arg0 itm
                                :node/return-type void})]
     inst-id)))

(defrecord ASet [arr idx val]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [arr-id (write-ssa arr)
      idx-id (write-ssa idx)
      val-id (write-ssa val)
      inst-id (add-instruction :inst.type/aset
                               {:inst.arg/arg0 arr-id
                                :inst.arg/arg1 idx-id
                                :inst.arg/arg2 val-id})]
     inst-id)))

(defrecord AGet [arr idx]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [arr-id (write-ssa arr)
      idx-id (write-ssa idx)
      inst-id (add-instruction :inst.type/aget
                               {:inst.arg/arg0 arr-id
                                :inst.arg/arg1 idx-id})]
     inst-id)))

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
          cptr (build (->Cast (->PointerType etp) ptr))
          gep (llvm/BuildStructGEP *builder* cptr idx (genname "set_"))]
      (llvm/BuildStore *builder* (build val) gep)
      bptr))
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [ptr-id (write-ssa ptr)
      val-id (write-ssa val)
      inst-id (add-instruction :inst.type/set
                               {:inst.arg/arg0 ptr-id
                                :inst.arg/arg1 val-id
                                :inst.set/member member})]
     ptr-id)))


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
          cptr (build (->Cast (->PointerType etp) ptr))
          gep (llvm/BuildStructGEP *builder* cptr idx (genname "get_"))] 
      (llvm/BuildLoad *builder* gep (genname "load_"))))
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [ptr-id (write-ssa ptr)
      inst-id (add-instruction :inst.type/get
                               {:inst.arg/arg0 ptr-id
                                :inst.get/member member})]
     inst-id)))


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

(defrecord Recur [items]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [item-ids (add-all (map write-ssa items))
      this-block (get-block)
      phis (get-binding :recur-phis)
      _ (add-all (map (fn [phi val]
                        (add-to-phi phi this-block val))
                      phis
                      item-ids))
      recur-pnt (get-binding :recur)
      _ (terminate-block :inst.type/jmp recur-pnt)]
     nil)))

(defrecord Do [body]
  SSAWriter
  (write-ssa [this]
    (gen-plan
     [body-ids (add-all (map write-ssa body))]
     (last body-ids))))

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
    (llvm/AddGlobalInAddressSpace *module*
                    (llvm-type type)
                    name
                    (target/default-address-space *target*))))

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
  SSAWriter
  (write-ssa [this]
    (write-ssa (->Const *int-type* this))))

(extend-type java.lang.Double
  SSAWriter
  (write-ssa [this]
    (write-ssa (->Const *float-type* this))))




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

