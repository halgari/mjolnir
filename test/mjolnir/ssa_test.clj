(ns mjolnir.ssa-test
  (:require
   [mjolnir.config :refer [*int-type*]]
   [mjolnir.ssa :refer :all]
   [mjolnir.types :refer [Int64 Int32 Float64 Float32 Float32* Float64*
                          ->FunctionType]]
   [mjolnir.expressions :refer [->Fn]]
   [midje.sweet :refer :all]))

(with-state-changes [(around :facts (binding [*db-conn* (new-db)
                                              *int-type* Int64] ?form))]
  (facts "IntegerType"
    (fact "IntegerType is a singleton"
      (-> (new-plan *db-conn*)
          (add-to-plan Int64)
          (add-to-plan Int64)
          :singletons
          count)
      =>
      1)
    (fact "IntegerType singletons respect existng data"
      (-> (new-plan *db-conn*)
          (add-to-plan Int64)
          commit)
      (-> (new-plan *db-conn*)
          (add-to-plan Int64)
          :new-ents
          count)
      =>
      0
      (-> (new-plan *db-conn*)
          (add-to-plan Int64)
          :singletons
          count)
      =>
      1)
    (fact "IntegerType will re-assert different widths"
      (-> (new-plan *db-conn*)
          (add-to-plan Int64)
          (add-to-plan Int32)
          :new-ents
          count)
      =>
      2))

  (facts "FloatType"
    (fact "FloatType is singleton"
      (-> (new-plan *db-conn*)
          (add-to-plan Float64)
          (add-to-plan Float64)
          :new-ents
          count)
      =>
      1)
    (fact "FloatsTypes are not IntTypes"
      (-> (new-plan *db-conn*)
          (add-to-plan Float64)
          (add-to-plan Int64)
          :new-ents
          count)
      =>
      2)
    (fact "FloatType will re-assert different widths"
      (-> (new-plan *db-conn*)
          (add-to-plan Float64)
          (add-to-plan Float32)
          :new-ents
          count)
      =>
      2))
  
  (facts "PointerType"
    (fact "PointerType is singleton"
      (-> (new-plan *db-conn*)
          (add-to-plan Float64*)
          (add-to-plan Float64*)
          :new-ents
          count)
      =>
      2
      ))

  (let [ft (->FunctionType [Float64 Int64] Int64)]
    (facts "FunctionType"
      (fact "FunctionType is singleton"
        (-> (new-plan *db-conn*)
            (add-to-plan ft)
            (add-to-plan ft)
            :new-ents
            count)
        =>
        5)
      (fact "Functions have proper return types"
        (let [plan (-> (new-plan *db-conn*)
                       (add-to-plan ft)
                       commit)]
          (:type.fn/return (plan-ent plan ft))
          =>
          (plan-ent plan Int64)))
      #_(fact "Functions have proper arguments"
          (to-seq (:type.fn/arguments (to-datoms ft)))
          =>
          [(to-datoms Float64)
           (to-datoms Int64)])))
  
  #_(let [ft (->FunctionType [Int64] Int64)
          f (->Fn "ret0" ft ["a" "b"]
                  0)]
      (facts "Fn"
        (fact "Functions create new entities each time"
          (to-datoms f)
          =not=>
          (to-datoms f))
        (fact "Functions with the same types are the same types"
          (:fn/type (to-datoms f))
          =>
          (:fn/type (to-datoms f)))
        (fact "Functions assert names"
          (->> (to-datoms f)
               :fn/argument-names
               to-seq
               (map :argument/name))
          =>
          ["a" "b"])
        (fact "Functions assert indexes"
          (->> (to-datoms f)
               :fn/argument-names
               to-seq
               (map :argument/idx))
          =>
          [0 1])))
  #_(facts "Long"
      (fact "Can be transacted"
        (:const/int-value (to-datoms 0))
        =>
        0)))