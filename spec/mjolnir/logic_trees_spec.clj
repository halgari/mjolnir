(ns mjolnir.logic-trees-spec
  (:require [speclj.core :refer :all]
            [mjolnir.logic-trees :refer :all]
            [clojure.core.logic :refer :all]))

(defrecord Baz [v c])

(describe "logic trees"
          (with t {:a [(->Baz 42 9)
                       [1 2]]
                   :b 44})
          (it "can create key paths"
              (should= #{[]
                         [:a]
                         [:a 0]
                         [:a 1]}
                       (set (get-key-paths
                             @t))))
          
          (it "can tag nodes with ids"
              (let [{:keys [tree ids]} (assign-ids @t)]
                (doseq [[path id] ids]
                  (should= id
                           (-> (get-in tree path)
                               meta
                               :id)))))
          (it "can construct indexes"
              (should (gen-index @t)))
          (it "can run basic attr queries"
              (should= 2
                       (-> (query @t
                                  [?v]
                                  (fresh [id]
                                         (tree id 0 ?v)))
                           count)))
          (it "can run complex queries"
              (should= 1
                       (-> (query @t
                                  [attr]
                                  (fresh [id pid]
                                         (tree id 0 1)
                                         (tree pid attr id)))
                           count))))