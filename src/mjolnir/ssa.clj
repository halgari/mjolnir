(ns mjolnir.ssa
  (:require [datomic.api :refer [q db] :as d]
            [clojure.pprint :refer [pprint]]))

(defn debug [x]
  (pprint x)
  x)

(def ^:dynamic *db-conn* nil)

(comment

  ssa-format
  [ARG_0 :ARG 0]
  [ARG_1 :ARG 1]
  [MUL_202 :MUL ARG_0 ARG_0]
  [MUL_203 :MUL ARG_1 ARG_1]
  [ADD_204 :ADD MUL_202 MUL_203]
  [FN_205 :GET-GLOBAL "llvm.sqrt"]
  [RET_205 :CALL FN_205 MUL_204]
  
  )

(def kw->attrs
  {:one [:db/cardinality :db.cardinality/one]
   :many [:db/cardinality :db.cardinalty/many]
   :ref [:db/valueType :db.type/ref]
   :keyword [:db/valueType :db.type/keyword]
   :int [:db/valueType :db.type/long]
   :string [:db/valueType :db.type/string]
   :unique [:db/unique :db.unique/value]})


(defn default-schema []
  {:list/tail #{:one :ref}
   :fn/type #{:one :ref}
   :fn/argument-names #{:one :ref}
   :fn/name #{:one :string}
   :fn/body #{:one :ref}
   :fn/entry-block #{:one :ref}

   :inst/block #{:one :ref}
   :inst/next #{:one :ref}
   :inst/type #{:one :keyword}

   :inst/return-value #{:one :ref}
   
   :block/fn #{:one :ref}

   :const/int-value #{:one :int}
   :const/type #{:one :ref}
   
   :argument/name #{:one :string}
   :argument/idx #{:one :int}

   :type.fn/return #{:one :ref}
   :type.fn/arguments #{:one :ref}

   :list/head #{:one :ref} 

   :node/type #{:one :keyword}
   :node/return-type #{:one :ref}
   
   :type/width #{:one :int}

   :type/element-type #{:one :ref}

   :error/key #{:one :keyword}
   :error/calue #{:one :string}

   })

(defn error-messages []
  {:error.fn/return-type-match "Return instructions must return the same type as their eclosing function"})

;; rules

(def error-message-rules
  '[[(error-message ?id ?err)
     [?x :error/key ?id]
     [?x :error/value ?err]]])

(def return-type-rules
  '[[(return-type ?id ?type)
     [?id :inst/type :inst.type/const]
     [?id :const/type ?type]]])

(def validation-error-rules
  (concat
   #_error-message-rules
   return-type-rules
   '[[(validation-error ?id ?ref ?err)
      [?id :node/type :node.type/fn]
      [?id :fn/type ?fn-type]
      [?fn-type :type.fn/return ?ret-type]
      [?block :block/fn ?id]
      [?ref :inst/block ?block]
      (return-type ?ref ?type)
      [(not= ?type ?ret-type)]
      [["Return instruction types must match function return type"] [?err]]]]))

(defn assert-schema [conn desc]
  (->> (for [[id attrs] desc]
         (merge
          {:db/id (d/tempid :db.part/db)
           :db/ident id
           :db.install/_attribute :db.part/db}
          (reduce
           (fn [m attr]
             (apply assoc m (kw->attrs attr)))
           {}
           attrs)))
       (d/transact conn)
       deref))

(defn get-query [sing]
  `[:find ~'?id
    :where
    ~@(map (fn [[k v]]
             (vector '?id k v))
           sing)])

(defn find-singleton [db sing]
  (assert db)
  (ffirst (q (get-query sing) db)))

(defrecord TxPlan [conn db singletons new-ents tempids])


(defn new-plan [conn]  (->TxPlan conn (db conn) {} {} {}))

(defn commit
  "Commit processes the transaction with the associated connection, then updates all the tempids to match. You can then use plan-id to get the realized ent-ids"
  [{:keys [conn db new-ents updates] :as plan}]
  (assert (and conn db))
  (let [data (concat (map
                      (fn [[ent id]]
                        (assoc ent :db/id id))
                      new-ents)
                     (map
                      (fn [[e a v]]
                        [:db/add e a v])
                      updates))
        {:keys [db-before db-after tempids tx-data]}
        @(d/transact conn data)
        ptempids (zipmap
                  (keys (:tempids plan))
                  (map (partial d/resolve-tempid db-after tempids)
                       (vals (:tempids plan))))]
    (assoc plan
      :tempids ptempids
      :db db-after
      :db-before db-before
      :new-ents nil
      :singletons nil)))

(defn plan-id 
  [plan val]
  (if-let [v (get-in plan [:tempids val])]
    v
    (assert false (str "Can't find " val))))

(defn plan-ent
  [plan val]
  (d/entity (:db plan) (get-in plan [:tempids val])))

(defn to-seq [head]
  (when head
    (cons (:list/head head)
          (lazy-seq (to-seq (:list/tail head))))))

(defn singleton [sing key]
  (fn [plan]
    (if-let [id (get-in plan [:singletons sing])]
      [id plan]
      (if-let [q (find-singleton (:db plan) sing)]
        [q (assoc-in plan [:singletons sing] q)]
      (let [newid (d/tempid :db.part/user)]
        [newid (-> plan
                   (assoc-in [:singletons sing] newid)
                   (assoc-in [:new-ents sing] newid)
                   (assoc-in [:tempids key] newid))])))))

(defn assert-entity [ent key]
  (fn [plan]
    (let [newid (d/tempid :db.part/user)]
      [newid (-> plan
                 (assoc-in [:new-ents ent] newid)
                 (assoc-in [:tempids key] newid))])))

(defn update-entity [ent attr val]
  (fn [plan]
    [ent (update-in plan [:updates] (fnil conj []) [ent attr val])]))

(defn add-all [itms]
  (fn [plan]
    (reduce
     (fn [[ids plan] f]
       (let [[id plan] (f plan)]
         [(conj ids id) plan]))
     [[] plan]
     itms)))

(defn assert-all [ents keys]
  (fn [plan]
    (reduce
     (fn [[ids plan] [ent key]]
       (let [[id plan] ((assert-entity ent key) plan)]
            [(conj ids id) plan]))
     [[] plan]
     (map vector ents keys))))

(defn- with-bind [id expr psym body]
  `(fn [~psym]
     (let [[~id ~psym] ( ~expr ~psym)]
       ~body)))

(defmacro gen-plan [binds id-expr]
  (let [binds (partition 2 binds)
        psym (gensym "plan_")
        f (reduce
           (fn [acc [id expr]]
             `(~(with-bind id expr psym acc)
               ~psym))
           `[~id-expr ~psym]
           (reverse binds))]
    `(fn [~psym]
       ~f)))

(defn get-plan [planval conn]
  (assert (ifn? planval))
  (let [val (planval (new-plan conn))]
    (assert (vector? val))
    (second val)))

(defn- assert-list-node [last id]
  (gen-plan
   [ent-id (let [ent (merge
                      (if last
                        {:list/tail last}
                        {})
                      {:list/head id})]
             (singleton ent ent))]
   ent-id))

(defn assert-seq [seq]
  (fn [plan]
    (reduce
     (fn [[last-id plan] id]
       ((assert-list-node last-id id) plan))
     [nil plan]
     (reverse seq))))

;; True ssa stuff here, blocks instructions etc.
(defn add-block [plan fn]
  (let [blk {:block/fn fn}
        new-plan (assert-entity plan blk blk)]
    [new-plan (plan-id new-plan blk)]))

(defn set-block [plan block-id]
  (assoc plan :block-id block-id))

(defn add-entry-block [plan fn]
  (let [[new-plan blk-id] (add-block plan fn)
        with-update (update-entity new-plan fn :fn/entry-block blk-id)]
    [with-update blk-id]))

(defn add-instruction
  ([{:keys [block-id prev-instruction-id] :as plan} instruction key attrs-map]
     (add-instruction plan block-id prev-instruction-id instruction key attrs-map))
  ([plan block-id prev-instruction-id instruction key attrs-map]
     (let [after-inst (assert-entity
                       plan
                       (merge
                        {:inst/block block-id
                         :inst/type instruction}
                        attrs-map)
                       key)
           inst-id (plan-id after-inst key)]
       (-> (if prev-instruction-id
             (update-entity after-inst prev-instruction-id :inst/next inst-id)
             after-inst)
           (assoc :prev-instruction-id inst-id)))))


(defn validation-errors [db]
  (q '[:find ?id ?ref ?err
       :in $ %
       :where
       (validation-error ?id ?ref ?err)]
     db
     validation-error-rules))


(defn new-db []
  (let [url (str "datomic:mem://ssa" (name (gensym)))]
    (d/create-database url)
    (let [conn (d/connect url)]
      (assert-schema conn (default-schema))
      conn)))


(defprotocol IToPlan
  (add-to-plan [this]
    "assert this item as datoms into the db and return the id of this entity"))

(comment
  (defmulti print-type :node/type)

  (defmethod print-node :type.fn
    (println (str "fn-type(" )))

  (defn print-fn [f]
    (println "Fn: " (:fn/name f)))

  (defn print-module [plan]
    (let [fns (->> (q '[:find ?id
                        :where [?id :fn/name ?name]]
                      (:db plan))
                   (mapv (comp (partial d/entity (:db plan))
                               first)))]
      (doseq [fn fns]
        (print-fn fn)))))

#_(defn -main []
  (to-datomic-schema (default-schema))
  (println (transact-new conn {:node/type :type/int
                               :type.int/width 32}))
  (println (transact-singleton conn {:node/type :type/int
                                     :type.int/width 32})))