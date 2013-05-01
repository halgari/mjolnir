(ns mjolnir.inference
  (:require [mjolnir.ssa :as ssa]
            [mjolnir.ssa-rules :refer [rules]]
            [datomic.api :refer [q db] :as d]))


(defn get-inferences [db]
  (q '[:find ?id ?attr ?val
       :in $ %
       :where
       [?id :node/type _]
       [(datomic.api/entity $ ?id) ?ent]
       [(:node/return-type ?ent) ?rval]
       [(nil? ?rval)]
       (infer-node ?id ?attr ?val)]
     db
     @rules))

(defn infer-all [conn]
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
    (d/transact conn data)))
