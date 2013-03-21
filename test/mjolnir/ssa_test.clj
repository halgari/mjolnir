(ns mjolnir.ssa-test
  (:require
   [datomic.api :refer [q db] :as d]
   [mjolnir.config :refer [*int-type*]]
   [mjolnir.ssa :refer :all]
   #_[mjolnir.llvm-builder :refer :all]
   [mjolnir.types :refer [Int64 Int32 Float64 Float32 Float32* Float64*
                          ->FunctionType]]
   [mjolnir.expressions :refer [->Fn]]
   [midje.sweet :refer :all]))

(with-state-changes [(around :facts (binding [*db-conn* (new-db)
                                              *int-type* Int64] ?form))]
  (facts "IntegerType"
    (fact "IntegerType is a singleton"
      (-> (gen-plan
           [int-id (add-to-plan Int64)]
           int-id)
          (get-plan *db-conn*)
          :singletons
          count)
      =>
      1)
    (fact "IntegerType singletons respect existng data"
      (-> (gen-plan
           [int-id (add-to-plan Int64)]
           int-id)
          (get-plan *db-conn*)
          commit
          :new-ents
          count)
      =>
      0
      (-> (gen-plan
           [int-id (add-to-plan Int64)]
           int-id)
          (get-plan *db-conn*)
          :singletons
          count)
      =>
      1)
    (fact "IntegerType will re-assert different widths"
      (-> (gen-plan
           [int64-id (add-to-plan Int64)
            int32-id (add-to-plan Int32)]
           0)
          (get-plan *db-conn*)
          :new-ents
          count)
      =>
      2)
    #_(fact "Can create llvm IntTypes"
        (-> (new-plan *db-conn*)
            (add-to-plan Int64)
            commit
            (plan-ent Int64)
            build-type)
        =not=>
        nil)

    (facts "FloatType"
      (fact "FloatType is singleton"
        (-> (gen-plan
             [f1 (add-to-plan Float64)
              f2 (add-to-plan Float64)]
             [f1 f2])
            (get-plan *db-conn*)
            :new-ents
            count)
        =>
        1)
      (fact "FloatsTypes are not IntTypes"
        (-> (gen-plan
             [f1 (add-to-plan Float64)
              i1 (add-to-plan Int64)]
             [i1 f1])
            (get-plan *db-conn*)
            :new-ents
            count)
        =>
        2)
      (fact "FloatType will re-assert different widths"
        (-> (gen-plan
             [f1 (add-to-plan Float64)
              f2 (add-to-plan Float32)]
             [f1 f2])
            (get-plan *db-conn*)
            :new-ents
            count)
        =>
        2)
      #_(fact "Can create llvm FloatTypes"
          (-> (new-plan *db-conn*)
              (add-to-plan Float64)
              commit
              (plan-ent Float64)
              build-type)
          =not=>
          nil)
      
      (facts "PointerType"
        (fact "PointerType is singleton"
          (-> (gen-plan
               [f1 (add-to-plan Float64*)
                f2 (add-to-plan Float64*)]
               [f1 f2])
              (get-plan *db-conn*)
              :new-ents
              count)
          =>
          2
          ))

      (let [ft (->FunctionType [Float64 Int64] Int64)]
        (facts "FunctionType"
          (fact "FunctionType is singleton"
            (-> (gen-plan
                 [f1 (add-to-plan ft)
                  f2 (add-to-plan ft)]
                 [f1 f2])
                (get-plan *db-conn*)
                :new-ents
                count)
            =>
            5)
          
          (fact "Functions have proper return types"
            (let [plan (-> (gen-plan
                            [ft-id (add-to-plan ft)]
                            ft-id)
                           (get-plan *db-conn*)
                           commit)]
              (:type.fn/return (plan-ent plan ft))
              =>
              (plan-ent plan Int64)))
          
          (fact "Functions have proper arguments"
            (let [plan (-> (gen-plan
                            [ft-id (add-to-plan ft)]
                            ft-id)
                           (get-plan *db-conn*)
                           commit)]
              (to-seq (:type.fn/arguments (plan-ent plan ft)))
              =>
              [(plan-ent plan Float64) (plan-ent plan Int64)]))
          
          (comment (fact "Can create llvm FunctionTypes"
                     (-> (new-plan *db-conn*)
                         (add-to-plan ft)
                         commit
                         (plan-ent ft)
                         build-type)
                     =not=>
                     nil)
                   
                   (let [ft (->FunctionType [Int64] Int64)
                         f (->Fn "ret0" ft ["a" "b"]
                                 0)]
                     (facts "Fn"
                       (fact "Functions can be commited"
                         (-> (new-plan *db-conn*)
                             (add-to-plan f)
                             commit
                             (plan-ent f))
                         =not=>
                         nil)
                       (fact "Functions have an entry block"
                         (-> (new-plan *db-conn*)
                             (add-to-plan f)
                             commit
                             (plan-ent f)
                             :fn/entry-block)
                         =not=>
                         nil)
                       (fact "Functions create new entities"
                         (-> (new-plan *db-conn*)
                             (add-to-plan f)
                             :new-ents
                             count)
                         =>
                         11)
                       (fact "Functions assert argument indexes"
                         (let [plan (-> (new-plan *db-conn*)
                                        (add-to-plan f)
                                        commit)]
                           (->> (plan-ent plan f)
                                :fn/argument-names
                                to-seq
                                (map :argument/idx))
                           =>
                           [0 1]))
                       (fact "Functions assert argument names"
                         (let [plan (-> (new-plan *db-conn*)
                                        (add-to-plan f)
                                        commit)]
                           (->> (plan-ent plan f)
                                :fn/argument-names
                                to-seq
                                (map :argument/name))
                           =>
                           ["a" "b"]))
                       (fact "Good functions validate without errors"
                         (let [plan (-> (new-plan *db-conn*)
                                        (add-to-plan f)
                                        commit)]
                           (validation-errors (db *db-conn*))
                           =>
                           #{}))))
                   #_(facts "Long"
                       (fact "Can be transacted"
                         (:const/int-value (to-datoms 0))
                         =>
                         0))))))))