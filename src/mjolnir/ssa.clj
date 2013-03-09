(ns mjolnir.ssa
  (:require [datomic.api :refer [q db] :as d]))

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
   :ref [:db/valueType :db.type/ref]
   :keyword [:db/valueType :db.type/keyword]
   :int [:db/valueType :db.type/long]})


(defn default-schema []
  {:list/tail #{:one :ref}
   :fn/type #{:one :ref}

   :type.fn/return #{:one :ref}
   :type.fn/arguments #{:one :ref}

   :list/head #{:one :ref} 

   :node/type #{:one :keyword}
   :type/width #{:one :int}

   :type/element-type #{:one :ref}

   })

(defn debug [x]
  (doseq [v x]
    (println v))
  x)

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

(defn transact-new [conn ent]
  (let [ent (if-not (:db/id ent)
              (assoc ent :db/id (d/tempid :db.part/user))
              ent)
        {:keys [db-after tempids]} @(d/transact conn [ent])
        tid (:db/id ent)]
    (->> (d/resolve-tempid db-after tempids tid)
         (d/entity db-after))))

(defn transact-singleton [conn sing]
  (let [genq (get-query sing)]
    (println genq "\n " sing "\n \n")
    (if-let [id (ffirst (q genq
                         (db conn)))]
      (d/entity (db conn) id)
      (transact-new conn sing))))

(defn transact-seq [conn seq]
  (reduce (fn [acc x]
            (transact-singleton
             conn
             (merge
              (if-let [id (:db/id acc)]
                {:list/tail id}
                {})
              {:list/head x})))
          {}
          (reverse seq)))

(defn to-seq [e]
  (when-not (nil? e)
    (cons (:list/head e)
          (lazy-seq (to-seq (:list/tail e))))))

(defn new-db []
  (let [url (str "datomic:mem://ssa" (name (gensym)))]
    (d/create-database url)
    (let [conn (d/connect url)]
      (assert-schema conn (default-schema))
      conn)))


(defprotocol IToDatoms
  (-to-datoms [this conn]
    "assert this item as datoms into the db and return the id of this entity"))

(defn to-datoms
  ([d]
     (-to-datoms d *db-conn*))
  ([conn d]
     (-to-datoms d conn)))

#_(defn -main []
  (to-datomic-schema (default-schema))
  (println (transact-new conn {:node/type :type/int
                               :type.int/width 32}))
  (println (transact-singleton conn {:node/type :type/int
                                     :type.int/width 32})))