(ns mjolnir.types-test
  (:use clojure.test
        mjolnir.types))

(defmacro fails [body]
  `(thrown? AssertionError ~body))

(deftest primitive-tests
  (testing "integers"
    (is (fails (IntegerType "42")))
    (is (IntegerType 42))
    (is (validate (IntegerType 42)))
    (is (llvm-type (IntegerType 42)))))