(ns mjolnir.inference
  (:refer-clojure :exclude [==])
  (:require [mjolnir.ssa :as ssa]
            [mjolnir.ssa-rules :refer [rules]]
            [datomic.api :refer [q db] :as d]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.datomic :as ld]))

(defn get-inferences [db]
  (let [notype (ffirst (q '[:find ?id
                            :where
                            [?id :node/type :node.type/unknown]]
                          db))]
    (concat (q '[:find ?id ?attr ?val
                 :in $ % ?notype
                 :where
                 #_[?id :node/return-type ?notype]
                 (infer-node ?id ?attr ?val)]
               db
               @rules
               notype)
            #_(q '[:find ?id ?attr ?val
                   :in $ % ?notype
                   :where
                   #_[?id :node/return-type ?notype]
                   (infer-cast-node ?id ?attr ?val)]
                 db
                 @rules
                 notype)
            #_(q '[:find ?id ?attr ?val
                   :in $ % ?notype
                   :where
                   #_[?id :node/return-type ?notype]
                   (infer-binop-node ?id ?attr ?val)]
                 db
                 @rules
                 notype)
            #_(->> (q '[:find ?id ?val
                        :in $ %
                        :where
                        [?id :node/return-type ?notype]
                        (infer-phi-return-type ?id ?val)]
                      db
                      @rules)
                   (map
                    (fn [[id val]]
                      [id :node/return-type val]))))))

(defn infer-all [conn]
  (let [db-val (db conn)
        nodes (->> (get-inferences (db conn))
                   (remove (fn [[id attr val]]
                             (= val (attr (d/entity db-val id))))))
        data (map (fn [[id attr val]]
                    [:db/add id attr val])
                  nodes)]
    @(d/transact conn data)
    (let [db-val (db conn)
          remaining (concat (q '[:find ?id ?attr
                                 :where
                                 [?id :node/return-type ?tp]
                                 [?tp :node/type :node.type/unknown]
                                 [(identity :node/return-type) ?attr]]
                               db-val)
                            (q '[:find ?id ?attr
                                 :where
                                 [?id :inst.cast/type :inst.cast/unknown]
                                 [?id :node/return-type ?tp-to]
                                 [?tp-to :node/type ?data1]
                                 [(identity :inst.cast/type) ?attr]]
                               db-val))]
      (when-not (= 0 (count remaining))
        (println "Remaining nodes...")
        (println (pr-str (q '[:find ?nm
                              :where
                              [_ :fn/name ?nm]]
                            db-val)))
        (doseq [[id attr] remaining]
          (let [ent (d/entity db-val id)]
            (println
             (pr-str
              [(:inst/type ent)
               attr
               (:node/type (:node/return-type ent))
               (:node/type (:node/return-type (:inst.arg/arg0 ent)))
               (:inst/type (:inst.arg/arg0 ent))
               (:inst.gbl/name (:inst.arg/arg0 ent))
               (-> ent
                   :inst/block
                   :block/fn
                   :fn/name)
               (-> ent
                   :inst/block
                   :block/fn)]))))
        (assert false "inference fails")))))
