(ns mjolnir.ssa-test
  (:require
   [mjolnir.ssa :refer :all]
   [mjolnir.types :refer [Int64 Int32 Float64 Float32 Float32* Float64*
                          ->FunctionType]]
   [midje.sweet :refer :all]))

(with-state-changes [(around :facts (binding [*db-conn* (new-db)] ?form))]
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
                  (to-datoms Int64)]))))