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

(defn return-typeo [node return-type]
  (fn [a]
    (let [wnode (walk a node)
          wreturn-type (walk a return-type)]
      (println [(ground? wnode) (ground? wreturn-type)])
      (match [(ground? wnode) (ground? wreturn-type)]
             [true true] (let [v (->> *index*
                                      :id-path
                                      wnode
                                      (get-in *tree*))]
                           (when (and (exp/Expression? v)
                                      (not (exp/Module? v)))
                             (->> v
                              exp/return-type
                              idub
                              (unify a return-type))))
             
             [true false]  (let [v (->> *index*
                                        :id-path
                                        wnode
                                        (get-in *tree*))]
                             (when (and (exp/Expression? v)
                                        (not (exp/Module? v)))
                               (->> v
                                   exp/return-type
                                   idub
                                   vector
                                   (unify a [wreturn-type]))))
             [false true] (->> *index*
                               :path-id
                               (map (fn [[path id]]
                                      (let [v (get-in *tree* path)]
                                        (if (and (exp/Expression? v)
                                                 (not (exp/Module? v))
                                                 (-> v
                                                     exp/return-type
                                                     (= return-type)))
                                          id
                                          nil))))
                               (remove nil?)
                               (idub wnode)
                               (map #(unify a wnode %))
                               to-stream)
             [false false] (->> *index*
                                :path-id
                                (map (fn [[path id]]
                                       (let [val (get-in *tree* path)]
                                         (when (and (exp/Expression? val)
                                                    (not (exp/Module? val)))
                                           (unify a [wnode wreturn-type] [id (exp/return-type val)])))))
                                (remove nil?)
                                idub
                                to-stream)))))

(defn typeo [node tp]
  (fn [a]
    (let [wnode (walk a node)
          wtp (walk a tp)]
      (match [(ground? wnode) (ground? wtp)]
             [false true] (->> *index*
                               :path-id
                               (map (fn [[path id]]
                                      (let [v (get-in *tree* path)]
                                        (when (= (class v) wtp)
                                          (unify a wnode id)))))
                               (remove nil?)
                               to-stream
                               idub)
             [true false] (let [path ((*index* :id-path) wnode)
                                n (get-in *tree* path)]
                            (unify a wtp (class n)))
             [_ _] (assert false (pr-str wnode wtp))))))


(defn matches-gc-type? [gc-type tp]
  (when (tp/pointer-type? tp)
    (let [gtp (->> tp
                   tp/etype
                   (iterate :extends)
                   (take-while (complement nil?))
                   last
                   :gc
                   :type)]
      (println "----->>>>>>>>>>>>>>>--" gtp gc-type)
      (= gtp gc-type))))

