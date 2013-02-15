(ns mjolnir.code-queries
  (:require [mjolnir.logic-trees :refer [ground? fresh? tree *index* *tree*]]
            [clojure.core.logic :refer :all]
            [mjolnir.expressions :as exp]
            [mjolnir.types :as tp]
            [clojure.core.match :refer [match]])
  (:import [mjolnir.types Type]))

(extend-protocol IUninitialized
  Type
  (-uninitialized [this]
    (apply assoc this (interleave (keys this) (repeat nil)))))


(defn idub [& more]
  (println "idub ->>>>> " more)
  (last more))


(defn filter-map-relation [filter-fn map-fn]
  (fn [node result]
    (fn [a]
      (let [wnode (walk a node)
            wresult (walk a result)]
        (match [(ground? wnode) (ground? wresult)]
               [true false] (let [n (->> *index*
                                        :id-path
                                        wnode
                                        (get-in *tree*))]
                              (when (filter-fn n)
                                (unify a wresult (map-fn n))))
               
               [false true] (let [paths (-> *index*
                                            :path-id)]
                              (->> paths
                                   (map (fn [[path id]]
                                          (let [node (get-in *tree* path)]
                                            (when (and (filter-fn node)
                                                       (= (map-fn node)
                                                          wresult))
                                              (unify a wnode id)))))
                                   (remove nil?)
                                   to-stream))
               [true true] (let [path ((:id-path *index*) wnode)
                                 n (get-in *tree* path)]
                             (when (filter-fn n)
                               (let [r (map-fn n)]
                                 (when (= r wresult)
                                   (unify a wresult r)))))
               [false false] (->> *index*
                                  :path-id
                                  (map (fn [[path id]]
                                         (let [n (get-in *tree* path)]
                                           (when (filter-fn n)
                                             (unify a [wnode wresult] [id (map-fn n)])))))
                                  (remove nil?)
                                  to-stream)
               [_ _] (assert false (str wnode wresult)))))))


(def return-typeo (filter-map-relation #(and (exp/Expression? %)
                                             (not (exp/Module? %)))
                                       exp/return-type))


(def typeo (filter-map-relation identity class))


#_(def gc-typeo (filter-map-relation #(and (tp/pointer-type? %)
                                         (tp/StructType? (tp/etype %)))
                                   #(-> %
                                        tp/etype
                                        (iterate :extends)
                                        (take-while (complement nil?))
                                        last
                                        :gc
                                        :type)))

(defn parent-of-typeo [pid tp id]
  (fresh [attr]
         (conde
          [(tree pid attr id )
           (typeo pid tp)]
          [(fresh [cid ctp]
                  (tree cid attr id)
                  (typeo cid ctp)
                  (!= ctp tp)
                  (parent-of-typeo pid tp cid))])))

(defn matches-gc-type? [gc-type tp]
  (when (tp/pointer-type? tp)
    (let [gtp (->> tp
                   tp/etype
                   (iterate :extends)
                   (take-while (complement nil?))
                   last
                   :gc
                   :type)]
      #_(println "----->>>>>>>>>>>>>>>--" gtp gc-type)
      (= gtp gc-type))))

