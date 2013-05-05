(ns mjolnir.inference
  (:refer-clojure :exclude [==])
  (:require [mjolnir.ssa :as ssa]
            [mjolnir.ssa-rules :refer [rules]]
            [datomic.api :refer [q db] :as d]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.datomic :as ld]))

(defn get-inferences [db]
  (println "infering nodes...  ")
  (let [notype (ffirst (q '[:find ?id
                            :where
                            [?id :node/type :node.type/unknown]]
                          db))]
    (time (concat (q '[:find ?id ?attr ?val
                       :in $ % ?notype
                       :where
                       #_[?id :node/return-type ?notype]
                       (infer-node ?id ?attr ?val)]
                     db
                     @rules
                     notype)
                  #_ki(->> (q '[:find ?id ?val
                            :in $ %
                            :where
                            [?id :node/return-type ?notype]
                            (infer-phi-return-type ?id ?val)]
                          db
                          @rules)
                       (map
                        (fn [[id val]]
                          [id :node/return-type val])))))))

(defn infer-all [conn]
  (let [db-val (db conn)
        nodes (->> (time (get-inferences (db conn)))
                   (remove (fn [[id attr val]]
                             (= val (attr (d/entity db-val id))))))
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
    (let [remaining (q '[:find ?id
                                     :where
                                     [?id :node/return-type ?tp]
                                     [?tp :node/type :node.type/unknown]]
                       (db conn))]
      (when-not (= 0 (count remaining))
        (println "Remaining nodes...")
        (println (q '[:find ?nm
                      :where
                      [_ :fn/name ?nm]]
                    (db conn)))
        (doseq [node remaining]
          (let [ent (d/entity db-val (first node))]
            (println
             [(:inst/type ent)
              (:node/return-type (:inst/type ent))
              (:node/return-type (:inst/type ent))
              (:inst.gbl/name ent)])))
        (assert false "inference fails")))))
