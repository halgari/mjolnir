(ns mjolnir.code-queries-spec
  (:require [speclj.core :refer :all]
            [clojure.core.logic :refer [fresh]]
            [mjolnir.logic-trees :refer :all]
            [mjolnir.code-queries :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.constructors-init :as const])
  (:alias c mjolnir.constructors)
  (:import [mjolnir.expressions IAdd]))

(describe "can query return types"
          (with code (c/iadd (c/iadd 4 3) 1))
          (it "can query code"
              (println (associative? @code) (keys @code))
              (should= 2
                       (-> (query @code
                                  [?id]
                                  (fresh [?val]
                                         (tree ?id :a ?val)))
                           count)))
          (it "can query a node's return type"
              (println (associative? @code) (keys @code))
              (should= 2
                       (-> (query @code
                                  [?id]
                                  (fresh [?val]
                                         (tree ?id :a ?val)
                                         (return-typeo ?id Int64)))
                           count)))
          (it "can query all nodes that return a type"
              (println (associative? @code) (keys @code))
              (should= 2
                       (-> (query @code
                                  [?id]
                                  (return-typeo ?id Int64))
                           count)))
          (it "can query all nodes and their types"
              (should= 2
                       (-> (query @code
                                  [?id ?tp]
                                  (return-typeo ?id ?tp))
                           count)))
          (it "can filter results"
              (should= 0
                       (-> (query @code
                                  [?id]
                                  (fresh [?val]
                                         (tree ?id :a ?val)
                                         (return-typeo ?id Int32)))
                           count))))

(describe "can query node types"
          (with code (c/iadd (c/iadd 4 3) 3))
          (it "can find all nodes of a given type"
              (should= 2
                       (-> (query @code
                                  [?id]
                                  (typeo ?id IAdd))
                           count)))
          (it "can find the type of a node"
              (should= 2
                       (-> (query @code
                                  [?id ?tp]
                                  (typeo ?id IAdd)
                                  (typeo ?id ?tp))
                           count))))

