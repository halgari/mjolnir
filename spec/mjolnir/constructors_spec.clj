(ns mjolnir.constructors-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.constructors-init :as cinit]
            [clojure.pprint :refer [pprint]])
  (:alias c mjolnir.constructors))


(describe "constructors"
          (it "can create basic functions"
               [ (c/defn fib [Int32 x -> Int32]
                    (c/if (c/or (c/is x 0)
                              (c/is x 1))
                        x
                        (c/iadd (fib (c/isub x 1))
                                (fib (c/isub x 2)))))
                (println fib)]
              (print @cinit/registered-globals)))