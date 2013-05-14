(ns mjolnir.gc.boehm
  (:require [mjolnir.constructors-init :refer [defnf]]
            [mjolnir.types :refer :all]
            [mjolnir.inference :refer [infer-all]]
            [mjolnir.ssa :refer [no-type gen-plan assert-entity update-entity add-all get-plan commit add-to-plan]]
            [datomic.api :refer [db q] :as d]
            [mjolnir.gc :refer :all]
            [mjolnir.llvmc :as llvm]
            [mjolnir.llvm-builder :refer [unpack-args build-type]])
  (:alias c mjolnir.constructors)
  (:import [com.sun.jna Native Pointer Memory]))

(c/defn ^{:exact "GC_malloc"} ^:extern GC_malloc [IntT size -> IntT*])
(c/defn ^{:exact "GC_init"} ^:extern GC_init [-> VoidT])

#_(c/defn ^{:exact "___init_GC___"} init_gc [-> IntT]
  (GC_init)
  0)

(defrecord BoehmGC []
  GC
  (build-new [this d module builder fn inst defs]
    (unpack-args defs inst
                 [size]
                 (let [fnc (llvm/GetNamedFunction module "GC_malloc")
                       _ (assert fnc "Couldn't find GC_malloc")
                       cresult (llvm/BuildCall builder
                                               fnc
                                               (into-array Pointer [size])
                                               1
                                               (str "gc_malloc_" (:db/id inst)))]
                   (llvm/BuildBitCast builder
                                      cresult
                                      (build-type (:node/return-type inst))
                                      (str "gc_casted_" (:db/id inst))))))
  (add-globals [this conn]
    (-> (gen-plan
         [_ (add-to-plan (c/module '[mjolnir.gc.boehm]))]
         nil)
        (get-plan conn)
        commit)
    conn))

(defn convert-to-gc-call [ent]
  (let [prev-node (:db/id (first (:inst/_next ent)))
        this-id (:db/id ent)]
    (gen-plan
     [no-type-id (no-type)
      gbl-id (assert-entity {:node/type :node.type/inst
                             :inst/type :inst.type/gbl
                             :inst.gbl/name "GC_malloc"
                             :inst/next this-id
                             :node/return-type no-type-id})
      _ (update-entity this-id {:inst/type :inst.type/call
                                :inst.call/fn gbl-id})
      _ (update-entity prev-node {:inst/next gbl-id})]
     nil)))

(defn run-gc-pass [conn]
  (let [db-val (db conn)
        news (q '[:find ?id
                  :where
                  [?id :inst/type :inst.type/new]]
                db-val)]
    (-> (gen-plan
         [_ (add-all (->> news
                          (map (comp (partial d/entity db-val) first))
                          (map convert-to-gc-call)))]
         nil)
        (get-plan conn)
        commit)
    (infer-all conn)
    conn))


