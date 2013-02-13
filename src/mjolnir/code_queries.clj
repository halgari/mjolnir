(ns mjolnir.code-queries
  (:require [mjolnir.logic-trees :refer [ground? fresh? tree *index* *tree*]]
            [clojure.core.logic :refer :all]
            [mjolnir.expressions :as exp]
            [clojure.core.match :refer [match]]))

(defn idub [x]
  (println "idub ->>>>> " x)
  x)

(defn return-typeo [node return-type]
  (fn [a]
    (let [wnode (walk a node)
          wreturn-type (walk a return-type)]
      (println "Return-Typeo" wnode wreturn-type a)
      (match [(ground? wnode) (ground? wreturn-type)]
             [true true] (->> *index*
                              :id-path
                              wnode
                              (get-in *tree*)
                              exp/return-type
                              idub
                              (unify a return-type))
             
             [true false] (->> *index*
                               :id-path
                               wnode
                               (get-in tree)
                               exp/return-type
                               idub
                               (unify a wreturn-type))
             [false true] (->> *index*
                               :path-id
                               (map (fn [[path id]]
                                      (if (-> (get-in *tree* path)
                                              exp/return-type
                                              (= return-type))
                                        id
                                        nil)))
                               (remove nil?)
                               idub
                               (map #(unify a wnode %))
                               to-stream)))))

