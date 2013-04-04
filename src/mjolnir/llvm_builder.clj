(ns mjolnir.llvm-builder
  (:require [mjolnir.ssa :refer :all]
            [datomic.api :refer [db q] :as d]
            [mjolnir.llvmc :as llvm]
            [mjolnir.targets.target :as target]
            [mjolnir.config :refer :all]))

(defn- unpack-arg [defs instr idx nm]
  `[~nm (~defs (~(idx->arg idx) ~instr))])

(defmacro unpack-args [defs instr args & body]
  (let [frms (mapcat (fn [idx arg]
                       (unpack-arg defs instr idx arg))
                     (range)
                     args)]
    `(let [~@frms]
       ~@(map (fn [a idx]
                `(assert ~a (str "can't find " (~(idx->arg idx) ~instr)
                                 " "
                                 (:inst/type (~(idx->arg idx) ~instr))
                                 " "
                                 (:db/id (:inst/block ~instr))
                                 " "
                                 (:db/id (:phi/block (~(idx->arg idx) ~instr))))))
              args
              (range))
       (assoc
         ~defs
         ~instr
         (do ~@body)))))

(comment
  (unpack-args defs instr
               [lh rh]
               )

  )

;; Converts a type entity to a llvm type
(defmulti build-type (fn [x] (:node/type x)))

(defmulti build-instruction (fn [d module builder fn itm defs]
                              (println ":inst/type "
                                       (:inst/type itm)
                                       (:db/id itm))
                              (:inst/type itm)))

(defmulti build-terminator (fn [module builder fn inst defs]
                             (:inst/type inst)))


(defmethod build-type :default
  [x]
  (assert false (str "Don't know how to create llvm-type from " (:node/type x))))


;; Using the type entity (tp) encodes vall as that type
(defmulti encode-const (fn [tp val] (:node/type tp)))


(defmethod build-type :type/int
  [x]
  (llvm/IntType (:type/width x)))
(defmethod encode-const :type/int
  [tp val]
  (llvm/ConstInt (build-type tp) val))



(defmethod build-type :type/float
  [x]
  (case (:type/width x)
    32 (llvm/FloatType)
    64 (llvm/DoubleType)))



(defmethod encode-const :type/float
  [tp val]
  (llvm/ConstReal (build-type tp) val))






(defmethod build-type :type/fn
  [{return-type :type.fn/return
    arg-types :type.fn/arguments}]
  (llvm/FunctionType (build-type return-type)
                     (llvm/map-parr build-type (to-seq arg-types))
                     (count arg-types)
                     false))


(defn new-module []
  (llvm/ModuleCreateWithName "Mjolnir Module"))

(defmulti stub-global (fn [module itm]
                        (:node/type itm)))

(defmethod stub-global :node.type/fn
  [module {name :fn/name type :fn/type arguments :fn/arguments linkage :fn/linkage}]
  (let [f (llvm/AddFunction module name (build-type type))]
    (llvm/SetFunctionCallConv f (target/get-calling-conv *target*
                                                         (= :extern
                                                            linkage)))
    (llvm/SetLinkage f (llvm/kw->linkage :extern))
    f))

(defmulti build-item (fn [db module itm]
                       (:node/type itm)))


(def arg-kws
  [:inst.arg/arg0
   :inst.arg/arg1])


(defn args-seq [ent]
  (take-while (complement nil?) (map (partial get ent) arg-kws)))



(defn depends-on?
  "Returns true if blk1 requires the results of instructions in blk2"
  [blk1 blk2]
  (let [st (set (map (fn [x]
                       (or (:inst/block x)
                           (debug (:phi/block x))
                           (assert nil (str x))))
                     (debug (mapcat args-seq (instruction-seq blk1)))))]
    (println (:db/id blk1) " depends-> " (contains? st blk2) "  "(:db/id blk2) st)
    (contains? st blk2)))

(defn block-comparator [blk1 blk2]
  (cond
   (depends-on? blk1 blk2) 1
   (depends-on? blk2 blk1) -1
   :else 0))

(defn get-ordered-block-list
  "Gets a list of blocks for a function sorted by suggested build order"
  [fnc]
  (let [entry (:fn/entry-block fnc)]
    (->> (sort block-comparator (:block/_fn fnc))
         (remove #{entry})
         (cons entry))))

(defn- build-phi-nodes [blk builder defs]
  (reduce
   (fn [defs phi]
     (assert (:node/return-type phi))
     (println "phi " (:db/id phi) phi)
     (assoc defs
       phi
       (llvm/BuildPhi builder
                      (build-type (:node/return-type phi))
                      (str "phi_" (:db/id phi)))))
   defs
   (:phi/_block blk)))

(defn- link-phi-nodes [fnc defs]
  (doall (for [block (:block/_fn fnc)
               node (:phi/_block block)
               incoming (:phi.value/_node node)]
           (let [inst (:phi.value/value incoming)
                 inst-block (or (:inst/block inst)
                                (:phi/block inst))
                 llvm-block (defs inst-block)
                 llvm-inst (defs inst)
                 llvm-phi (defs node)]
             (assert llvm-phi)
             (assert llvm-inst)
             (assert llvm-block inst-block)
             (llvm/AddIncoming llvm-phi
                               (llvm/map-parr identity [llvm-inst])
                               (llvm/map-parr identity [llvm-block])
                               1)))))


(defn build-block [db-val module fnc block defs]
  (let [builder (llvm/CreateBuilder)
        llvm-block (llvm/AppendBasicBlock fnc (str "blk_" (:db/id block)))
        _ (llvm/PositionBuilderAtEnd builder llvm-block)
        defs (assoc defs block llvm-block)
        _ (println "_----")
        defs (build-phi-nodes block builder defs)
        defs (reduce
              (fn [defs inst]
                (build-instruction db-val module builder fnc inst defs))
              defs
              (instruction-seq block))]
    defs))

(defn build-termination [db-val module fnc block defs]
  (let [instr (:block/terminator-inst block)
        llvm-block (defs block)
        builder (llvm/CreateBuilder)]
    (assert (and instr llvm-block)
            (str "Looking for "
                 (:db/id (:block/terminator-inst block))
                 " and "
                 (:db/id block)))
    (llvm/PositionBuilderAtEnd builder llvm-block)
    (build-instruction db-val module builder fnc instr defs)))

(defmethod build-item :node.type/fn
  [db-val module this]
  (let [blocks (get-ordered-block-list this)
        _ (debug (map :db/id blocks))
        fnc (llvm/GetNamedFunction module (:fn/name this))
        defs (reduce
              (fn [defs block]
                (build-block db-val module fnc block defs))
              {}
              blocks)
        defs (reduce
              (fn [defs block]
                (build-termination db-val module fnc block defs))
              defs
              blocks)]
    (link-phi-nodes this defs)))


(defmethod build-instruction :default
  [d module builder fn itm defs]
  (assert false (pr-str "Can't build instruction " itm)))

(defmethod build-instruction :inst.type/const
  [d module builder fn itm defs]
  (assoc defs itm (encode-const (:const/type itm)
                                (or (:const/int-value itm)))))

(defmethod build-instruction :inst.type/arg
  [d module builder fn itm defs]
  (assoc defs itm
         (llvm/GetParam fn (:inst.arg/idx itm))))

(defn- gen-op-name [instr]
  (name (gensym (str (name (:inst.binop/type instr)) "_"))))

(def binop->llvm-binop
  {:inst.binop.subtype/iadd llvm/LLVMAdd
   :inst.binop.subtype/isub llvm/LLVMSub
   :inst.binop.subtype/imul llvm/LLVMMul
   :inst.binop.subtype/idiv llvm/LLVMSDiv
   :inst.binop.subtype/imod llvm/LLVMSRem
   :inst.binop.subtype/fadd llvm/LLVMFAdd
   :inst.binop.subtype/fsub llvm/LLVMFSub
   :inst.binop.subtype/fmul llvm/LLVMFMul
   :inst.binop.subtype/fdiv llvm/LLVMFDiv
   :inst.binop.subtype/fmod llvm/LLVMFRem})

(defmethod build-instruction :inst.type/binop
  [d module builder fn inst defs]
  (let [llvm-op (binop->llvm-binop (:inst.binop/sub-type inst))]
    (assert llvm-op (str "no map for:  " (:inst.binop/sub-type inst)))
    (unpack-args defs inst
                 [lh rh]
                 (llvm/BuildBinOp builder llvm-op lh rh (gen-op-name inst)))))

(defmethod build-instruction :inst.type/jmp
  [d module builder fn inst defs]
  (unpack-args defs inst
               [dest]
               (llvm/BuildBr builder dest)))

(defmethod build-instruction :inst.type/br
  [d module builder fn inst defs]
  (unpack-args defs inst
               [test then else]
               (llvm/BuildCondBr builder test then else)))



(defmethod build-instruction :inst.type/cmp
  [d module builder fn inst defs]
  (let [lh (defs (:inst.arg/arg0 inst))
        rh (defs (:inst.arg/arg1 inst))]
    (assert (and lh rh))
    (assoc defs
      inst
      (llvm/BuildICmp builder llvm/LLVMIntSLE lh rh (str "cmp_" (:db/id inst))))))

(defmethod build-instruction :inst.type/return-val
  [d module builder fn inst defs]
  (unpack-args defs inst
               [ret]
               (llvm/BuildRet builder
                              ret
                              (str "return_" (:db/id inst)))))

(defmethod build-instruction :inst.type/gbl
  [d module builder fn itm defs]
  (assert defs)
  (assoc defs
    itm
    (llvm/GetNamedFunction module (:inst.gbl/name itm))))

(defmethod build-instruction :inst.type/jmp
  [d module builder fn inst defs]
  (unpack-args defs inst
               [blk]
               (llvm/BuildBr builder blk)))

(defmethod build-instruction :inst.type/call
  [d module builder fn inst defs]
  (let [args (map defs (args-seq inst))
        fnc (defs (:inst.call/fn inst))]
    (assert (and (every? identity args) fnc))
    (->>
     (llvm/BuildCall builder fnc (llvm/map-parr identity args) (count args) (str (:db/id inst)))
     (assoc defs inst))))



(defn build [db]
  (let [globals (->> (q '[:find ?id
                          :in $ %
                          :where
                          (global-def ?id ?name ?type)]
                        db
                        ssa-rules)
                     (map (comp (partial d/entity db) first)))
        _ (println "Globals: " (count globals))
        module (new-module)
        builder (llvm/CreateBuilder)]
    (doseq [global globals]
      (stub-global module global))
    (doseq [global globals]
      (build-item db module  global))
    module))


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

