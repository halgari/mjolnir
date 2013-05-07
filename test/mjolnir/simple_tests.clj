(ns mjolnir.simple-tests
  (:require
   [mjolnir.inference :refer [infer-all]]
   [mjolnir.validation :refer [validate]]
   [clojure.test :refer :all]
   [datomic.api :refer [q db] :as d]
   [mjolnir.config :refer [*int-type* *target* default-target *float-type*]]
   [mjolnir.ssa :refer :all]
   [mjolnir.llvm-builder :refer :all]
   [mjolnir.types :refer [Int64 Int32 Float64 Float32 Float32* Float64*
                          ->FunctionType VoidT ->ArrayType]]
   [mjolnir.expressions :refer [->Fn ->Binop ->Arg ->If ->Call ->Gbl ->Cmp ->Let ->Local ->Loop ->Recur ->Free ->Malloc
                                ->ASet ->AGet ->Do ->Module]]
   [mjolnir.constructors-init :refer [defnf]]
   [mjolnir.core :refer [to-db to-llvm-module]])
  (:alias c mjolnir.constructors))


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
      (build (db conn)))))


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
      (let [x (build (db conn))]
        x))))



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
      (let [x (build (db conn))]
        x))))

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
      (let [x (build (db conn))]

        x))))

(deftest compile-aget-aset-function
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
      (let [x (build (db conn))]
        x))))

(deftest compile-module
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [] VoidT)
          atype (->ArrayType Int64 10)
          fnc (->Fn "fnc1" ft [] 1)
          fnc2 (->Fn "fnc2" ft [] 2)
          mod (->Module [fnc fnc2])]
      (-> (gen-plan
           [f-id (add-to-plan mod)]
           f-id)
          (get-plan conn)
          commit)
      (infer-all conn)
      (validate (db conn))
      (let [x (build (db conn))]
        x))))


(deftest compile-dotimes
  (binding [*int-type* Int64
            *target* (default-target)]
    (let [conn (new-db)
          ft (->FunctionType [Int64] Int64)
          fnc (->Fn "fib" ft ["x"]
                    (c/if (c/= 1 2)
                          (c/loop [x 42]
                                  42)
                          42))]
      (-> (gen-plan
           [f-id (add-to-plan fnc)]
           f-id)
          (get-plan conn)
          commit)
      (infer-all conn)
      (validate (db conn))
      (let [x (build (db conn))]
        (verify x)
        x))))


(defnf defnf-fib [Float64 x -> Float64]
  (if (< x 2.0)
    x
    (+ (defnf-fib (- x 1.0))
       (defnf-fib (- x 2.0)))))

(deftest compile-constructors
  (binding [*int-type* Int64
            *float-type* Float64
            *target* (default-target)]
    (-> (c/module ['mjolnir.simple-tests/defnf-fib])
        to-db
        to-llvm-module)))



(defnf for-test [Float32 max -> Float32]
  (for [x [0.0 max 1.0]]
    1.0)
  1.0)

(deftest for-tests
  (binding [*float-type* Float32
            *target* (default-target)]
    (-> (c/module ['mjolnir.simple-tests/for-test])
        to-db
        to-llvm-module)))



(c/defn for-max [Float32 xpx Float32 ypx Float32 max Float32 width Float32 height -> Float32]
  (c/for [x [0.0 max 1.0]]
    1.0)
  1.0)

(deftest for-max
  (binding [*float-type* Float32
            *target* (default-target)]
    (-> (c/module ['mjolnir.simple-tests/for-max])
        to-db
        to-llvm-module)))


;; Float math test - mandelbrot

(defnf square [Float32 x -> Float32]
  (* x x))



(defnf calc-iteration [Float32 xpx Float32 ypx Float32 max Float32 width Float32 height -> Float32]
  (let [x0 (- (* (/ xpx width) 3.5) 2.5)
        y0 (- (/ (/ ypx height) 2.0) 1.0)]
    (loop [iteration 0.0
           x 0.0
           y 0.0]
      (if (and (< (+ (square x)
                     (square y))
                  (square 2.0))
               (< iteration max))
        (recur (+ iteration 1.0)
               (+ (- (square x)
                     (square y))
                  x0)
               (+ (* 2.0 x y)
                  y0))
        iteration))))

(defnf ^:extern calc-mandelbrot [Float32* arr Float32 width Float32 height Float32 max -> Float32*]
  (for [y [0.0 height 1.0]]
    (for [x [0.0 width 1.0]]
      (let [idx (cast Int64 (+ (* y width) x))]
        (aset arr idx (/ (calc-iteration x y max width height) max)))))
  arr)


#_(deftest compile-mandelbrot
  (binding [*int-type* Int64
            *float-type* Float32
            *target* (default-target)]
    (-> (c/module ['mjolnir.simple-tests/square
                   'mjolnir.simple-tests/calc-iteration
                   'mjolnir.simple-tests/calc-mandelbrot])
        to-db
        to-llvm-module)))


