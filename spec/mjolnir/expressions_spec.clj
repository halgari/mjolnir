(ns mjolnir.expressions-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]))


(describe "ConstInteger"
          (it "can be created"
              (should (->ConstInteger 42 Int32)))
          (it "can be validated"
              (should (valid? (->ConstInteger 42 Int32)))
              (should-throw (valid? (->ConstInteger 42 "foo"))))
          (it "can get a return type"
              (should= Int32
                       (return-type (->ConstInteger 42 Int32)))))

(describe "Function"
          (it "can compile"
              (should (->> (->ConstInteger 42 Int32)
                           (->Fn "foo" (->FunctionType [] Int32) [])
                           vector
                           (->Module "TestModule")
                           compile-module))))