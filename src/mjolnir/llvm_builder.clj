(ns mjolnir.llvm-builder
  (:require [mjolnir.ssa :refer :all]
            [datomic.api :refer [db q] :as d]
            [mjolnir.llvmc :as llvm]
            [mjolnir.targets.target :as target]
            [mjolnir.config :refer :all])
  (:import [com.sun.jna Native Pointer]))

(defn- unpack-arg [defs instr idx nm]
  `[~nm (~defs (~(idx->arg idx) ~instr))])

(defmacro unpack-args [defs instr args & body]
  (let [frms (mapcat (fn [idx arg]
                       (unpack-arg defs instr idx arg))
                     (range)
                     args)]
    `(let [~@frms]
       ~@(mapcat (fn [idx a]
                   [`(let [blk# (or (:phi/block (~(idx->arg idx) ~instr))
                                    (:inst/block (~(idx->arg idx) ~instr)))
                           blk-inst# (~defs blk#)]
                       #_(assert blk-inst# (str "Block not built: " blk# " in " (:inst/block ~instr))))
                    `(assert  ~a
                              (str "can't find {" (~(idx->arg idx) ~instr)
                                   " "
                                   (:inst/type (~(idx->arg idx) ~instr))
                                   " "
                                   "} for { "
                                   (:inst/type ~instr)
                                   " "
                                   (:db/id (:inst/block ~instr))
                                   "} "
                                   (:db/id (:phi/block (~(idx->arg idx) ~instr)))))
                    `(assert (not (map? ~a))
                             (str "bad arg format " (~(idx->arg idx) ~instr)
                                  " "
                                  (:inst/type (~(idx->arg idx) ~instr))
                                  " "
                                  (:db/id (:inst/block ~instr))
                                  "->  "
                                  ~(pr-str a)
                                  ~a))])
                 (range)
                 args)
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
(defmulti build-type :node/type)

(defmulti build-instruction (fn [d module builder fn itm defs]
                              (:inst/type itm)))

(defmulti build-terminator (fn [module builder fn inst defs]
                             (:inst/type inst)))


(defmethod build-type :default
  [x]
  (assert false (str "Don't know how to build-type from " (pr-str x))))


;; Using the type entity (tp) encodes vall as that type
(defmulti encode-const (fn [tp val] (:node/type tp)))


(defmethod build-type :type/int
  [x]
  (llvm/IntType (:type/width x)))

(defmethod build-type :type/float
  [x]
  (llvm/FloatType (:type/width x)))

(defmethod encode-const :type/int
  [tp val]
  (llvm/ConstInt (build-type tp) val))

(defmethod build-type :type/void
  [x]
  (llvm/VoidType))



(defmethod build-type :type/float
  [x]
  (case (:type/width x)
    32 (llvm/FloatType)
    64 (llvm/DoubleType)))

(defmethod build-type :type/array
  [x]
  (let [etype (build-type (:type/element-type x))]
    (llvm/ArrayType etype (:type/length x))))

(defmethod build-type :type/pointer
  [x]
  (let [etype (build-type (:type/element-type x))]
    (llvm/PointerType etype
                      0)))



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
  (let [st (-> (set (map (fn [x]
                           (or (:inst/block x)
                               (:phi/block x)))
                         (concat (mapcat args-seq (instruction-seq blk1))
                                 (args-seq (:block/terminator-inst blk1)))))
               (disj nil))]
    (contains? st blk2)))

(defn find-a-node [deps already-have-nodes]
  (some (fn [[k v]] (if (empty? (remove already-have-nodes v)) k)) deps))

(defn order-nodes [deps]
  (loop [deps deps already-have-nodes #{} output []]
    (if (empty? deps)
      output
      (if-let [item (find-a-node deps already-have-nodes)]
        (recur
          (dissoc deps item)
          (conj already-have-nodes item)
          (conj output item))
        (throw (Exception. "Circular dependency."))))))

(defn node-deps [blocks block]
  (->> blocks
       (remove #{block})
       (filter (partial depends-on? block))))

(defn block-comparator [blk1 blk2]
  (let [result (cond
                (depends-on? blk1 blk2) 1
                (depends-on? blk2 blk1) -1
                :else 0)]
    result))

(defn get-unbuilt-deps [block blocks defs]
  (when-let [dep-blk (->>
                      blocks
                      (remove defs)
                      (filter (partial depends-on? block blocks))
                      first)]
    (get-unbuilt-deps dep-blk)
    block))

(defn get-ordered-block-list
  "Gets a list of blocks for a function sorted by suggested build order"
  [fnc]
  (let [entry (:fn/entry-block fnc)
        blocks (:block/_fn fnc)
        deps (zipmap blocks
                     (map (partial node-deps blocks) blocks))]
    (->> #_(sort block-comparator (:block/_fn fnc))
         #_(block-order (first blocks) #{} (set blocks))
         (order-nodes deps)
         (remove #{entry})
         (cons entry))))

(defn- build-phi-nodes [blk builder defs]
  (reduce
   (fn [defs phi]
     (assert (:node/return-type phi))
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
                 inst-block (:phi.value/block incoming)
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
        llvm-block (llvm/AppendBasicBlock fnc (str (:block/name block) "_" (:db/id block)))
        _ (llvm/PositionBuilderAtEnd builder llvm-block)
        defs (assoc defs block llvm-block)
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
  (when-not (:fn/extern? this)
    (let [blocks (get-ordered-block-list this)
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
      (link-phi-nodes this defs))))


(defmethod build-instruction :default
  [d module builder fn itm defs]
  (assert false (pr-str "Can't build instruction " (d/touch itm))))

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
   :inst.binop.subtype/fmod llvm/LLVMFRem
   :inst.binop.subtype/and llvm/LLVMAnd
   :inst.binop.subtype/or llvm/LLVMOr})

(defmethod build-instruction :inst.type/binop
  [d module builder fn inst defs]
  (let [llvm-op (binop->llvm-binop (:inst.binop/sub-type inst))]
    (assert llvm-op (str "no binop map for:  "
                         (:inst.binop/sub-type inst)
                         " "
                         (:inst.binop/type inst)))
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


(def cmp-map
  {[:type/int :type/int :inst.cmp.pred/=] :inst.cmp.sub-pred/int-eq
   [:type/int :type/int :inst.cmp.pred/not=] :inst.cmp.sub-pred/int-ne
   [:type/int :type/int :inst.cmp.pred/>] :inst.cmp.sub-pred/int-sgt
   [:type/int :type/int :inst.cmp.pred/<] :inst.cmp.sub-pred/int-slt
   [:type/int :type/int :inst.cmp.pred/<=] :inst.cmp.sub-pred/int-sle
   [:type/int :type/int :inst.cmp.pred/>=] :inst.cmp.sub-pred/int-sge

   [:type/float :type/float :inst.cmp.pred/=] :inst.cmp.sub-pred/real-oeq
   [:type/float :type/float :inst.cmp.pred/not=] :inst.cmp.sub-pred/real-one
   [:type/float :type/float :inst.cmp.pred/>] :inst.cmp.sub-pred/real-ogt
   [:type/float :type/float :inst.cmp.pred/<] :inst.cmp.sub-pred/real-olt
   [:type/float :type/float :inst.cmp.pred/<=] :inst.cmp.sub-pred/real-ole
   [:type/float :type/float :inst.cmp.pred/>=] :inst.cmp.sub-pred/real-oge})

(def cmp-table
  {:inst.cmp.sub-pred/int-eq llvm/LLVMIntEQ
   :inst.cmp.sub-pred/int-ne llvm/LLVMIntNE
   :inst.cmp.sub-pred/int-ugt llvm/LLVMIntUGT
   :inst.cmp.sub-pred/int-uge llvm/LLVMIntUGE
   :inst.cmp.sub-pred/int-ult llvm/LLVMIntULT
   :inst.cmp.sub-pred/int-ule llvm/LLVMIntULE
   :inst.cmp.sub-pred/int-sgt llvm/LLVMIntSGT
   :inst.cmp.sub-pred/int-sge llvm/LLVMIntSGE
   :inst.cmp.sub-pred/int-slt llvm/LLVMIntSLT
   :inst.cmp.sub-pred/int-sle llvm/LLVMIntSLE

   :inst.cmp.sub-pred/real-predicate-false llvm/LLVMRealPredicateFalse
   :inst.cmp.sub-pred/real-oeq llvm/LLVMRealOEQ
   :inst.cmp.sub-pred/real-ogt llvm/LLVMRealOGT
   :inst.cmp.sub-pred/real-oge llvm/LLVMRealOGE
   :inst.cmp.sub-pred/real-ole llvm/LLVMRealOLE
   :inst.cmp.sub-pred/real-olt llvm/LLVMRealOLT
   :inst.cmp.sub-pred/real-one llvm/LLVMRealONE
   :inst.cmp.sub-pred/real-ord llvm/LLVMRealORD
   :inst.cmp.sub-pred/real-uno llvm/LLVMRealUNO
   :inst.cmp.sub-pred/real-ueq llvm/LLVMRealUEQ
   :inst.cmp.sub-pred/real-ugt llvm/LLVMRealUGT
   :inst.cmp.sub-pred/real-uge llvm/LLVMRealUGE
   :inst.cmp.sub-pred/real-ult llvm/LLVMRealULT
   :inst.cmp.sub-pred/real-ule llvm/LLVMRealULE
   :inst.cmp.sub-pred/real-une llvm/LLVMRealUNE
   :inst.cmp.sub-pred/real-predicate-true llvm/LLVMRealOEQ})

(defmethod build-instruction :inst.type/cmp
  [d module builder fn inst defs]
  (unpack-args defs inst
               [lh rh]
               (let [lh-t (-> inst :inst.arg/arg0 :node/return-type :node/type)
                     rh-t (-> inst :inst.arg/arg1 :node/return-type :node/type)
                     pred (-> inst :inst.cmp/pred)
                     sub-type (-> (cmp-map [lh-t rh-t pred])
                                  cmp-table)]
                 (assert (integer? sub-type) (pr-str "Invalid cmp type" [sub-type
                                                              lh-t
                                                              rh-t
                                                              pred
                                                              (cmp-map [lh-t rh-t pred])
                                                              (-> (cmp-map [lh-t rh-t pred])
                                                                  cmp-table)]))
                 (cond
                  (= lh-t rh-t :type/int)
                  (llvm/BuildICmp builder sub-type lh rh (str "cmp_" (:db/id inst)))

                  (= lh-t rh-t :type/float)
                  (llvm/BuildFCmp builder sub-type lh rh (str "cmp_" (:db/id inst)))

                  :else (assert false "No LLVM predicate builder")))))

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


(defmethod build-instruction :inst.type/malloc
  [d module builder fn inst defs]
  (let [tp (:inst.malloc/type inst)
        llvm-type (build-type tp)]
    (->> (llvm/BuildMalloc builder llvm-type (str "malloc_" (:db/id inst)))
         (assoc defs inst))))

(defmethod build-instruction :inst.type/free
  [d module builder fn inst defs]
  (unpack-args defs inst
               [ptr]
               (llvm/BuildFree builder ptr)))

(defn- pointer-type-to [x]
  {:node/type :type/pointer
   :type/element-type x})


(defmethod build-instruction :inst.type/aset
  [d module builder fn inst defs]
  (unpack-args defs inst
               [ptr idx val]
               (let [ret-type (-> inst
                                  :inst.arg/arg0
                                  :node/return-type
                                  :type/element-type
                                  pointer-type-to
                                  build-type)
                 casted (llvm/BuildBitCast builder ptr ret-type (str "casted_" (:db/id inst)))
                 gep (llvm/BuildGEP builder
                                    casted
                                    (into-array Pointer [idx])
                                    1
                                    (str "gep_" (:db/id inst)))]
                 (llvm/BuildStore builder val gep)
                 gep)))

(defmethod build-instruction :inst.type/aget
  [d module builder fn inst defs]
  (unpack-args defs inst
               [ptr idx]
               (let [ptr-type (-> inst
                                  :inst.arg/arg0
                                  :node/return-type
                                  :type/element-type
                                  pointer-type-to
                                  build-type)
                     casted (llvm/BuildBitCast builder ptr ptr-type (str "casted_" (:db/id inst)))
                     gep (llvm/BuildGEP builder
                                        casted
                                        (into-array Pointer [idx])
                                        1
                                        (str "gep_" (:db/id inst)))]
                 (llvm/BuildLoad builder gep (str "load_" (:db/id inst))))))

(def cast-table
  {:inst.cast.type/fp-to-si llvm/LLVMFPToSI
   :inst.cast.type/si-to-fp llvm/LLVMSIToFP
   :inst.cast.type/trunc llvm/LLVMTrunc
   :inst.cast.type/zext llvm/LLVMZExt
   :inst.cast.type/bitcast llvm/LLVMBitcast
   :inst.cast.type/ptr-to-int llvm/LLVMPtrToInt
   :inst.cast.type/int-to-ptr llvm/LLVMIntToPtr})

(defmethod build-instruction :inst.type/cast
  [d module builder fn inst defs]
  (unpack-args defs inst
               [val]
               (let [to-type (-> inst
                                 :node/return-type
                                 build-type)
                     sub-type (cast-table (:inst.cast/type inst))]
                 (assert sub-type (str "Unknown subtype for" inst))
                 (llvm/BuildCast builder sub-type val to-type (str "cast_" (:db/id inst))))))



(defn build [db]
  (let [globals (->> (q '[:find ?id
                          :in $ %
                          :where
                          (global-def ?id ?name ?type)]
                        db
                        ssa-rules)
                     (map (comp (partial d/entity db) first)))
        module (new-module)
        builder (llvm/CreateBuilder)]
    (doseq [global globals]
      (stub-global module global))
    (doseq [global globals]
      (build-item db module  global))
    module))


(defn verify [module]
  (let [ptr (llvm/new-pointer)]
    (when-not (= (llvm/VerifyModule module llvm/LLVMPrintMessageAction ptr) false)
      (assert false #_(.getString ptr 0)))
    (llvm/DisposeMessage (llvm/value-at ptr))
    module))


(defn optimize [module]
  (let [pass (llvm/CreatePassManager)]
    (llvm/AddDefaultPasses pass)
    (llvm/RunPassManager pass module)
    (llvm/DisposePassManager pass)
    #_(llvm/DumpModule module)
    module))

(defn dump [module]
  (llvm/DumpModule module))

