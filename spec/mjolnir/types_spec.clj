(ns mjolnir.types-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]))

(describe "IntegerType"
          (it "can be created"
              (should (->IntegerType 32)))
          (it "validates"
              (should (valid? (->IntegerType 32)))
              (should-throw (valid? (->IntegerType "foo"))))
          (it "can convert to llvm type"
              (should (llvm-type (->IntegerType 32)))))

(describe "PointerType"
          (it "can be created"
              (should (->PointerType (->IntegerType 32))))
          (it "validates"
              (should (valid? (->PointerType (->IntegerType 32))))
              (should-throw (valid? (->PointerType 42))))
          (it "can get etype"
              (should= (->IntegerType 32)
                       (etype (->PointerType (->IntegerType 32)))))
          (it "can convert to llvm type"
              (should (llvm-type (->PointerType (->IntegerType 42))))))

(describe "FunctionType"
          (it "can be created"
              (should (->FunctionType [Int32 Int32] I8*)))
          (it "validates"
              (should (valid? (->FunctionType [Int32] I8*)))
              (should-throw (valid? (->FunctionType [44] I8*))))
          (it "can convert to llvm type"
              (should (llvm-type (->FunctionType [Int32 Int64] I8*)))))