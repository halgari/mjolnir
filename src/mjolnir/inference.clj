(ns mjolnir.inference
  (:refer-clojure :exclude [==])
  (:require [mjolnir.ssa :as ssa]
            [mjolnir.ssa-rules :refer [rules]]
            [datomic.api :refer [q db] :as d]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.datomic :as ld]))

(comment
  (def ^:dynamic *db*)


  (defn query [v]
    (fn [a]
      #_(println v)
      ((ld/query *db* v) a)))

  (declare return-type)

  (defn global-def [?id ?name ?type]
    (query [?id :node/type :node.type/fn])
    (query [?id :fn/name ?name])
    (query [?id :fn/type ?type]))

  (defn default-return-type
    "If a type already exists, use it"
    [?id ?type]
    (query [?id :node/return-type ?type]))

  (defn consts-return-type
    "Consts return their given type, if it exists"
    [?id ?type]
    (query [?id :inst/type :inst.type/const])
    (query [?id :const/type ?type]))

  (defn binop-return-type
    "Binops return the same type as their args"
    [?id ?type]
    (fresh [?arg0 ?arg1]
           (query [?id :inst/type :inst.type/binop])
           (query [?id :inst.arg/arg0 ?arg0])
           (query [?id :inst.arg/arg1 ?arg1])
           (return-type ?arg0 ?type)
           (return-type ?arg1 ?type)))

  (defn phi-return-type [?id ?type]
    (fresh [?phi ?arg]
           (query [?phi :phi.value/value ?arg])
           (query [?phi :phi.value/node ?id])
           (return-type ?arg ?id)))

  (defn global-return-type [?id ?type]
    (fresh [?name ?gbl]
           (query [?id :inst/type :inst.type/gbl])
           (query [?id :inst.gbl/name ?name])
           (global-def ?gbl ?name ?type)))

  (defn call-return-type [?id ?type]
    (fresh [?fn-src ?fn-t]
           (query [?id :inst/type :inst.type/call])
           (query [?id :inst.call/fn ?fn-src])
           (return-type ?fn-src ?fn-t)
           (query [?fn-t :type.fn/return ?type])))

  (defn arg-return-type [?id ?type]
    (fresh [?block ?fn ?fn-t ?idx ?arg-node]
           (query [?id :inst/type :inst.type/arg])
           (query [?id :inst/block ?block])
           (query [?block :block/fn ?fn])
           (query [?fn :fn/type ?fn-t])
           (query [?arg-node :fn.arg/fn ?fn-t])
           (query [?id :inst.arg/idx ?idx])
           (query [?arg-node :fn.arg/idx ?idx])
           (query [?arg-node :fn.arg/type ?type])))

  (defn aset-return-type [?id ?type]
    (fresh [?arg0]
           (query [?id :inst/type :inst.type/aset])
           (query [?id :inst.arg/arg0 ?arg0])
           (return-type ?arg0 ?type)))

  (defn aget-return-type [?id ?type]
    (fresh [?arg0 ?arg0-t]
           (query [?id :inst/type :inst.type/aget])
           (query [?id :inst.arg/arg0 ?arg0])
           (return-type ?arg0 ?arg0-t)
           (query [?arg0-t :type/element-type ?type])))

  (def return-type
    (tabled [?id ?type]
            (conde
             [(default-return-type ?id ?type)]
             [(consts-return-type ?id ?type)]
             [(binop-return-type ?id ?type)]
             [(phi-return-type ?id ?type)]
             [(global-return-type ?id ?type)]
             [(call-return-type ?id ?type)]
             [(arg-return-type ?id ?type)]
             [(aset-return-type ?id ?type)]
             [(aget-return-type ?id ?type)]))))

(declare return-type)

(defn e-for-av [db a v]
  (d/entity db (ffirst (q '[:find ?id
                            :in $ ?a ?v
                            :where
                            [?id ?a ?v]]
                          db a v))))

(defn debug [x]
  (println x)
  x)

(defmulti -return-type (fn [db ent]
                         (if (:node/return-type ent)
                           :default
                           (let [tp (:node/type ent)]
                             (if (= tp :node.type/inst)
                               (:inst/type ent)
                               tp)))))

(defmethod -return-type :default
  [db ent]
  (:node/return-type ent))

(defmethod -return-type :inst.type/const
  [db ent]
  (:const/type ent))


(defmethod -return-type :inst.type/binop
  [db ent]
  (let [arg0-t (return-type db (:inst.arg/arg0 ent))
        arg1-t (return-type db (:inst.arg/arg1 ent))]
    (when (= arg0-t arg1-t)
      arg0-t)))

(defmethod -return-type :node.type/phi
  [db ent]
  (->> (map :phi.value/value (:phi.value/_node ent))
       (map (partial return-type db))
       (remove (partial = :started))
       first))

(defmethod -return-type :inst.type/gbl
  [db ent]
  (let [nm (:inst.gbl/name ent)
        gbl (e-for-av db :fn/name nm)]
    (:fn/type gbl)))

(defmethod -return-type :inst.type/call
  [db ent]
  (->> ent
       :inst.call/fn
       (return-type db)
       :type.fn/return))

(defmethod -return-type :inst.type/aset
  [db ent]
  (->> ent
       :inst.arg/arg0
       (return-type db)))

(defmethod -return-type :inst.type/aget
  [db ent]
  (->> ent
       :inst.arg/arg0
       (return-type db)
       :type/element-type))


(def ^:dynamic *cache*)
(def ^:dynamic *seen* #{})

(defn return-type [db ent]
  (if-let [cached (@*cache* ent)]
    cached
    (if (*seen* ent)
      :started
      (binding [*seen* (conj *seen* ent)]
        (let [tp (-return-type db ent)]
          (when-not (or (nil? tp)
                      (= tp :started))
            (swap! *cache* assoc ent tp))
          tp))))
  
  #_(if-let [res (get @*cache* ent)]
    res
    (do (swap! *cache* assoc ent :started)
        (let [tp (-return-type db ent)]
          (swap! *cache* assoc ent tp)
          (when tp
            (@*cache* ent))))))

(defn ents-to-infer [db]
  (->> (q '[:find ?id
            :where
            [?id :node/type ?type]]
          db)
       (map first)
       (map (partial d/entity db))))


(defn binop-translation-table [v]
  (println v)
  (get
   {[:type/int :type/int]     {:inst.binop.type/add :inst.binop.subtype/iadd
                               :inst.binop.type/sub :inst.binop.subtype/isub
                               :inst.binop.type/mul :inst.binop.subtype/imul
                               :inst.binop.type/div :inst.binop.subtype/idiv
                               :inst.binop.type/rem :inst.binop.subtype/irem}
    [:type/float :type/float] {:inst.binop.type/add :inst.binop.subtype/fadd
                               :inst.binop.type/sub :inst.binop.subtype/fsub
                               :inst.binop.type/mul :inst.binop.subtype/fmul
                               :inst.binop.type/div :inst.binop.subtype/fdiv
                               :inst.binop.type/rem :inst.binop.subtype/frem}}
   v))


(defn binop-subtypes [db]
  (time (let [all-binops (q '[:find ?id
                              :where
                              [?id :inst/type :inst.type/binop]]
                            db)
              infered-binops (q '[:find ?id :inst.binop/sub-type ?sub-t
                                  :where
                                  [?id :inst/type :inst.type/binop]
                                  [?id :inst.arg/arg0 ?arg0]
                                  [?id :inst.arg/arg1 ?arg1]
                                  [?arg0 :node/return-type ?arg0-t]
                                  [?arg1 :node/return-type ?arg1-t]
                                  [(vector ?arg0-t ?arg1-t) ?v]
                                  [(mjolnir.inference/binop-translation-table ?v) ?table]
                                  [?id :inst.binop/type ?binop]
                                  [(?table ?binop) ?sub-t]]
                                db)]
          (assert (= (count all-binops)
                     (count infered-binops)))
          infered-binops)))

(defn get-inferences [db]
  
  (println "infering nodes")
  (binding [*cache* (atom {})]
    (let [nodes (time (->> (ents-to-infer db)
                           (pmap (fn [x]
                                  [(:db/id x) :node/return-type (:db/id (return-type db x))]))
                           (remove (comp nil? last))
                           set))]
      nodes)))

(defn infer-all [conn]
  (dotimes [x 2] ; Due to loops we have to run this twice to get
                 ; all the nodes
    (let [db-val (db conn)
          nodes (->> (time (get-inferences (db conn)))
                     (remove (fn [[id attr _]]
                               (attr (d/entity db-val id)))))
          data (map (fn [[id attr val]]
                      [:db/add id attr val])
                    nodes)]
      (doseq [s (map (fn [[?nd ?attr ?type]]
                       [(:inst/type (d/touch (d/entity (db conn) ?nd))) :->
                        ?attr :->
                        ?type])
                     nodes)])
      (println "infered" (count nodes) "nodes")
      (d/transact conn data)
      (let [db-val (db conn)
            nodes (->> (time (get-inferences db-val))
                       (remove (fn [[id attr _]]
                                 (attr (d/entity db-val id))))
                       (map (fn [[id attr val]]
                              [:db/add id attr val])))]
        (println "subtyped " (count nodes))
        (d/transact conn data)))))
