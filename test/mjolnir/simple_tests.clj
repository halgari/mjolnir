(ns mjolnir.simple-tests
  (:require
   [mjolnir.inference :refer [infer-all]]
   [mjolnir.validation :refer [validate]]
   [clojure.test :refer :all]
   [datomic.api :refer [q db] :as d]
   [mjolnir.config :refer [*int-type* *target* default-target]]
   [mjolnir.ssa :refer :all]
   [mjolnir.llvm-builder :refer :all]
   [mjolnir.types :refer [Int64 Int32 Float64 Float32 Float32* Float64*
                          ->FunctionType VoidT ->ArrayType]]
   [mjolnir.expressions :refer [->Fn ->Binop ->Arg ->If ->Call ->Gbl ->Cmp ->Let ->Local ->Loop ->Recur ->Free ->Malloc
                                ->ASet ->AGet ->Do]]))


(deftest compile-add-one-function
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [Int64] Int64)
          add-one (->Fn "add-one" ft ["a"]
                        (->Binop :+ (->Arg 0) 1))]
      (-> (gen-plan
           [f-id (add-to-plan add-one)]
           f-id)
          (get-plan conn)
          commit)
      (dotimes [x 3] (infer-all conn))      
      (dump (build (db conn))))))


(deftest compile-fib-function
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [Int64] Int64)
          fib (->Fn "fib" ft ["x"]
                    (->If (->Cmp :<= (->Arg 0) 1)
                          (->Arg 0)
                          (->Binop :+
                                   (->Call (->Gbl "fib")
                                           [(->Binop :- (->Arg 0) 1)])
                                   (->Call (->Gbl "fib")
                                           [(->Binop :- (->Arg 0) 2)]))))]
      (-> (gen-plan
           [f-id (add-to-plan fib)]
           f-id)
          (get-plan conn)
          commit)
      (dotimes [x 3] (infer-all conn))
      (validate (db conn))
      (dump (let [x (build (db conn))]
              x)))))



(deftest compile-let-function
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [Int64] Int64)
          fnc (->Fn "fib" ft ["x"]
                    (->Let "foo" (->Arg 0)
                           (->Local "foo")))]
      (-> (gen-plan
           [f-id (add-to-plan fnc)]
           f-id)
          (get-plan conn)
          commit)
      (dotimes [x 3] (infer-all conn))
      (validate (db conn))
      (dump (let [x (build (db conn))]
              x)))))

(deftest compile-count-to-ten-function
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [Int64] Int64)
          fnc (->Fn "fib" ft ["x"]
                    (->Loop [["x" 10]]
                            (->If (->Cmp :< (->Local "x") 10)
                                  (->Recur [(->Binop :+ (->Local "x") 1)])
                                  (->Local "x"))))]
      (-> (gen-plan
           [f-id (add-to-plan fnc)]
           f-id)
          (get-plan conn)
          commit)
      (infer-all conn)
      (validate (db conn))
      (dump (let [x (build (db conn))]

              x)))))

(deftest compile-count-to-ten-function
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [] VoidT)
          atype (->ArrayType Int64 10)
          fnc (->Fn "fib" ft []
                    (->Let "arr" (->Malloc atype)
                           (->Do
                            [(->ASet (->Local "arr") 0 42)
                             (->AGet (->Local "arr") 0)
                             (->Free (->Local "arr"))])))]
      (-> (gen-plan
           [f-id (add-to-plan fnc)]
           f-id)
          (get-plan conn)
          commit)
      (infer-all conn)
      (validate (db conn))
      (dump (let [x (build (db conn))]
              x)))))





