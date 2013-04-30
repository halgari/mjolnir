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
   :boolean [:db/valueType :db.type/boolean]
   :string [:db/valueType :db.type/string]
   :unique [:db/unique :db.unique/value]})


(defn default-schema []
  {:list/tail #{:one :ref}
   :fn/type #{:one :ref}
   :fn/argument-names #{:one :ref}
   :fn/name #{:one :string}
   :fn/body #{:one :ref}
   :fn/entry-block #{:one :ref}
   :fn/extern? #{:one :boolean}

   :fn.arg/type #{:one :ref}
   :fn.arg/idx #{:one :int}
   :fn.arg/fn #{:one :ref}

   :inst/block #{:one :ref}
   :inst/next #{:one :ref}
   :inst/type #{:one :keyword}

   :phi/block #{:one :ref}
   :phi.value/node #{:one :ref}
   :phi.value/block #{:one :ref}
   :phi.value/value #{:one :ref}

   :inst/return-value #{:one :ref}
   
   :block/fn #{:one :ref}
   :block/terminator-inst #{:one :ref}
   
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
   :type/length #{:one :int}

   :error/key #{:one :keyword}
   :error/calue #{:one :string}

   :inst.binop/type #{:one :keyword}
   :inst.binop/sub-type #{:one :keyword}
   :inst.arg/idx #{:one :int}

   :inst.cast/type #{:one :ref}

   :inst.gbl/name #{:one :string}
   :inst.call/fn #{:one :ref}

   :inst.cmp/pred #{:one :keyword}

   :inst.malloc/type #{:one :ref}

   ;; args
   :inst.arg/arg0 #{:one :ref}
   :inst.arg/arg1 #{:one :ref}
   :inst.arg/arg2 #{:one :ref}
   })


(def idx->arg
  [:inst.arg/arg0
   :inst.arg/arg1
   :inst.arg/arg2])

(def bin-ops
  #{:inst.binop/+})

(defn error-messages []
  {:error.fn/return-type-match "Return instructions must return the same type as their eclosing function"})

;; rules

(defmacro defrule [name args & body])

(defrule global-def [?id]
  [?id :node/type :node.type/fn])



(def global-defs
  '[[(global-def ?id)
     [?id :node/type :node.type/fn]]
    [(global-def ?id)
     [?id :node/type :node.type/fn]]])

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

(def ssa-rules
  (concat global-defs))

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
  (assert db (pr-str db))
  (ffirst (q (get-query sing) db)))

(defrecord TxPlan [conn db singletons new-ents tempids])


#_(defn new-plan [conn]  )

(defn commit
  "Commit processes the transaction with the associated connection, then updates all the tempids to match. You can then use plan-id to get the realized ent-ids"
  [{:keys [conn db new-ents updates valid-ids] :as plan}]
  (assert (and conn db))
  (let [ents (reduce
              (fn [acc [ent id]]
                (assert (not (get acc id)) "Duplicate ids")
                (assoc acc id (assoc ent :db/id id)))
              {}
              new-ents)
        _ (assert (= (set (keys ents))
                     (set (keys valid-ids)))
                  (pr-str (count (set (keys ents)))
                          (count (set (keys valid-ids)))
                          (count new-ents)))
        data (-> (reduce
                  (fn [acc [k attr val]]
                    (assert (and k (get acc k)) (pr-str "Bad db-id given in update"
                                                        k
                                                        (get valid-ids k)
                                                        " in "
                                                        (keys valid-ids)))
                    (assoc-in acc [k attr] val))
                  ents
                  updates)
                 vals)
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

(defn singleton
  ([sing]
     (singleton sing nil))
  ([sing key]
      (fn [plan]
        (if-let [id (get-in plan [:singletons sing])]
          [id plan]
          (if-let [q (find-singleton (:db plan) sing)]
            [q (assoc-in plan [:singletons sing] q)]
            (let [newid (d/tempid :db.part/user)]
              [newid (-> plan
                         (assoc-in [:singletons sing] newid)
                         (assoc-in [:new-ents sing] newid)
                         (assoc-in [:tempids key] newid)
                         (assoc-in [:valid-ids newid] newid))]))))))

(defn assert-entity
  ([ent]
     (assert-entity ent nil))
  ([ent key]
      (fn [plan]
        (let [newid (d/tempid :db.part/user)
              ent (assoc ent :db/id newid)]
          [newid (-> plan
                     (assoc-in [:new-ents ent] newid)
                     (assoc-in [:tempids key] newid)
                     (assoc-in [:valid-ids newid] newid))]))))

(defn update-entity [ent & attrs-vals]
  (let [pairs (partition 2 attrs-vals)]
    (fn [plan]
      (assert (get (:valid-ids plan) ent) (pr-str "Must give entity id" ent "=>" (:valid-ids plan)))
      (let [new-plan (reduce
                      (fn [plan [k v]]
                        (update-in plan [:updates] (fnil conj []) [ent k v]))
                      plan
                      pairs)]
        [ent new-plan]))))

(defn update-all [itms]
  (fn [plan]
    (let [new-plan (reduce
                    (fn [plan data]
                      (update-in plan [:updates] (fnil conj []) data))
                    plan
                    itms)]
      [nil new-plan])))

(defn add-all [itms]
  (fn [plan]
    (reduce
     (fn [[ids plan] f]
       (let [[id plan] (f plan)]
         [(conj ids id) plan]))
     [[] plan]
     itms)))

(defn assert-all [ents]
  (fn [plan]
    (reduce
     (fn [[ids plan] [ent key]]
       (let [[id plan] ((assert-entity ent key) plan)]
            [(conj ids id) plan]))
     [[] plan]
     ents)))

(defn- with-bind [id expr psym body]
  `(fn [~psym]
     (let [[~id ~psym] ( ~expr ~psym)]
       (assert ~psym "Nill plan")
       ~body)))

(defmacro gen-plan [binds id-expr]
  (let [binds (partition 2 binds)
        psym (gensym "plan_")
        forms (reduce
               (fn [acc [id expr]]
                 (concat acc `[[~id ~psym] (~expr ~psym)]))
               []
               binds)]
    `(fn [~psym]
       (let [~@forms]
         [~id-expr ~psym]))))

(defn assoc-plan [key val]
  (fn [plan]
    [nil (assoc plan key val)]))

(defn assoc-in-plan [path val]
  (fn [plan]
    [nil (assoc-in plan path val)]))

(defn get-in-plan [path]
  (fn [plan]
    [(get-in plan path) plan]))

(defn push-binding [key value]
  (fn [plan]
    [nil (update-in plan [:bindings key] conj value)]))

(defn push-alter-binding [key f & args]
  (fn [plan]
    [nil (update-in plan [:bindings key]
                  #(conj % (apply f (first %) args)))]))

(defn get-binding [key]
  (fn [plan]
    [(first (get-in plan [:bindings key])) plan]))

(defn pop-binding [key]
  (fn [plan]
    [(first (get-in plan [:bindings key]))
     (update-in plan [:bindgins key] pop)]))

(defn no-op []
  (fn [plan]
    [nil plan]))

(defn get-plan [planval conn]
  (assert (ifn? planval))
  (let [val (planval (->TxPlan conn (db conn) {} {} {}))]
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
(defn add-block [fn]
  (let [blk {:block/fn fn}]
    (gen-plan
     [blk (assert-entity blk blk)]
     blk)))

(defn mark-extern-fn [fn-id]
  (gen-plan
   [_ (update-entity fn-id :fn/extern? true)]
   nil))

(defn set-block [block-id]
  (fn [plan]
    [block-id (assoc plan :block-id block-id)]))

(defn add-entry-block [fn-id]
  (gen-plan
   [blk (add-block fn-id)
    _ (assoc-plan :block-id blk)
    _ (update-entity fn-id :fn/entry-block blk)]
   blk))

(defn get-block []
  (fn [plan]
    [(:block-id plan) plan]))

(defn add-phi
  "Adds a phi node to a block. In Mjolnir phi nodes are always attached to the start of a block.
The order of the nodes cannot be set, as it shouldn't matter in the output seimantics of the code"
  []
  (println "phi.......")
  (gen-plan
   [block (get-block)
    phi-id (assert-entity {:node/type :node.type/phi
                           :phi/block block
                           :inst/type :inst.type/phi})]
   phi-id))

(defn add-to-phi
  "adds an incomming value to a phi node"
  [phi-node block-id value]
  (gen-plan
   [val (assert-entity {:node/type :node.type/phi-value
                        :phi.value/node phi-node
                        :phi.value/block block-id
                        :phi.value/value value})]
   val))

(defn terminate-block
  "Sets the terminator instruction for a block"
  [inst & args]
  (gen-plan
   [block (get-block)
    inst (assert-entity (reduce
                         (fn [acc [idx arg]]
                           (assoc acc (idx->arg idx) arg))
                         {:inst/type inst}
                         (map vector
                              (range)
                              args)))
    _ (assoc-in-plan [:state block :terminated] true)
    _ (update-entity block :block/terminator-inst inst)]
   block))

(defn terminated?
  [block]
  (gen-plan
   [term (get-in-plan [:state block])]
   term))

(defn add-instruction
  ([instruction attrs-map]
     (add-instruction instruction attrs-map nil))
  ([instruction attrs-map key]
     (fn [plan]
       (let [block-id (:block-id plan)
             prev-instruction-id (get-in plan [:block-states block-id :prev-instruction-id])]
         ((add-instruction block-id prev-instruction-id instruction attrs-map key) plan))))
  ([block-id prev-instruction-id instruction attrs-map key]
     (gen-plan
      [inst-id (assert-entity
                (merge
                 {:inst/block block-id
                  :inst/type instruction
                  :node/type :node.type/inst}
                 attrs-map)
                       key)
       _ (if prev-instruction-id
           (update-entity prev-instruction-id :inst/next inst-id)
           (no-op))
       _ (assoc-in-plan [:block-states block-id :prev-instruction-id] inst-id)]
      inst-id)))

(defn instruction-seq [block]
  ;; Get all the instructions, bounce up to the top, then return a seq
  ;; of all
  (->> (:inst/_block block)
       (map d/touch)
       first
       (iterate (comp first :inst/_next))
       (take-while (complement nil?))
       last
       (iterate :inst/next)
       (take-while (complement nil?))))


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