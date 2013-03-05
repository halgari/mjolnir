(ns mjolnir.nvptx-spec
  (:require [speclj.core :refer :all]
            [mjolnir.targets.nvptx :as nvptx]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.constructors-init :as cinit]
            [mjolnir.transforms.ref-count :as rc]
            [mjolnir.logic-trees :as lt]
            [clojure.pprint :refer [pprint]]
            [mjolnir.targets.target :refer [emit-to-file]]
            )
  (:alias c mjolnir.constructors))

(c/defn get-val [Int64 x Int64 y -> Int64]
  (c/+ x y))

#_(c/defn vec-add [Int64 n Float32* a Float32* b Float32* c -> Int64]
  (c/let [i (c/+ (c/* (ptx/blockIdx_x)
                      (ptx/blockDim_x))
                 (ptx/threadIdx_x))]
         (c/if (c/< i n)
               (c/aset sum
                       i
                       (c/+ (c/aget a i)
                            (c/aget b i)))))
  0)

(nvptx/init-target identity)

(describe "NVPTX Backend"
          (it "can be created"
              (should (nvptx/make-default-target)))
          (it "can write a simple fn"
              (should
               (emit-to-file
                (nvptx/make-default-target)
                (-> (c/module ['mjolnir.nvptx-spec/get-val])
                    build)
                {:filename "foo.ptx"
                 :obj-type :asm}))))