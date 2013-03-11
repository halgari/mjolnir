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
         (fact "IntegerType is singleton"
               (to-datoms Int64) => (to-datoms Int64))
         (fact "IntegerType will re-assert different widths"
               (to-datoms Int64) =not=> (to-datoms Int32)))

  (facts "FloatType"
         (fact "IntegerType is singleton"
               (to-datoms Float64) => (to-datoms Float64))
         (fact "IntegerType will re-assert different widths"
               (to-datoms Float64) =not=> (to-datoms Float32)))
  
  (facts "PointerType"
         (fact "IntegerType is singleton"
               (to-datoms Float64*) => (to-datoms Float64*))
         (fact "IntegerType will re-assert different widths"
               (to-datoms Float64*) =not=> (to-datoms Float32*))
         (fact "Element type matches"
               (:type/element-type (to-datoms Float64*)) => (to-datoms Float64)))

  (let [ft (->FunctionType [Float64 Int64] Int64)]
    (facts "FunctionType"
           (fact "FunctionType is singleton"
                 (to-datoms ft) => (to-datoms ft))
           (fact "Functions have proper return types"
                 (:type.fn/return (to-datoms ft))
                 =>
                 (to-datoms Int64))
           (fact "Functions have proper arguments"
                 (to-seq (:type.fn/arguments (to-datoms ft)))
                 =>
                 [(to-datoms Float64)
                  (to-datoms Int64)])))
  
  (let [ft (->FunctionType [Int64] Int64)
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
                 [0 1]))
    (facts "Long"
           (fact "Can be transacted"
                 (:const/int-value (to-datoms 0))
                 =>
                 0))))