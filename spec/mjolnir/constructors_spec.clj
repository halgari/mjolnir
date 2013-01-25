(ns mjolnir.constructors-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.constructors-init :as cinit]
            )
  (:alias c mjolnir.constructors))


 

(c/defn fib [Int32 x -> Int32]
                (c/if (c/or (c/is x 0)
                            (c/is x 1))
                      x
                      (c/iadd (fib (c/isub x 1))
                              (fib (c/isub x 2)))))

(def buf-size (* 1024 1024))

(c/defn cnt [Int32 x -> Int32]
  (c/using [frame (c/malloc Int32 buf-size)]
           #_(c/dotimes [y buf-size]
                      (c/aset frame y 1))
           #_(c/loop [i 0
                    idx 0]
                   (c/if (c/is idx buf-size)
                         i
                         (c/recur (c/iadd i (c/aget frame idx))
                                  (c/iadd idx 1)
                                  -> Int32)))))


;; Tests basic recursion
(c/defn cnt-to [Int32 final -> Int32]
  (c/loop [i 0]
          (c/if (c/is i final)
                i
                (c/recur (c/iadd i 1) -> Int32))))

;; Tests ASet/AGet
(c/defn aset-aget [Int32 num -> Int32]
  (c/using [data (c/malloc Int32 5)]
           (c/aget (c/aset data 0 num) 0)))

;; Tests to see if we can nex
(c/defn nested-dotimes [-> Int32]
  (c/dotimes [x 100]
             (c/dotimes [y 200]
                        (c/+ x y)
      #_(c/dotimes [z 300]
        (c/+ x y z)))))


(describe "constructors"
          (it "can create basic functions"
              (-> (c/module ['mjolnir.constructors-spec/fib])
                  (pdebug)
                  (valid?)))
          (it "can compile created functions"
              (let [m (c/module ['mjolnir.constructors-spec/fib])]
                (pdebug m)
                (valid? m)
                (build m)))
          (it "can execute"
              (let [m (c/module ['mjolnir.constructors-spec/fib])
                    _ (pdebug m)
                    _ (valid? m)
                    bm (build m)
                    e (engine bm)
                    f (get-fn e bm "mjolnir.constructors-spec/fib")]
                (f 3)
                (println (time (f 6)))))
          (it "can run count-to function"
              (should= 42
                       (let [m (c/module ['mjolnir.constructors-spec/cnt-to])
                             _ (pdebug m)
                             _ (valid? m)
                             bm (build m)
                             e (engine bm)
                             f (get-fn e bm "mjolnir.constructors-spec/cnt-to")]
                         (f 42))))
          (it "can run aget-aset function"
              (should= 42
                       (let [m (c/module ['mjolnir.constructors-spec/aset-aget])
                                _ (pdebug m)
                                _ (valid? m)
                             mb (build m)
                             e (engine mb)
                             f (get-fn e mb "mjolnir.constructors-spec/aset-aget")]
                         (f 42))))
          #_(it "can run cnt function"
              (let [m (c/module ['mjolnir.constructors-spec/cnt])
                    _ (pdebug m)
                    _ (valid? m)
                    mb (build m)
                    e (engine mb)
                    f (get-fn e mb "mjolnir.constructors-spec/cnt")
                    ]
                #_(write-object-file mb "x86-64")
                (time (f 32))
                (println e f mb)))
          (it "can nest dotimes"
              (let [m (c/module ['mjolnir.constructors-spec/nested-dotimes])
                    _ (valid? m)
                    mb (build m)
                    d (dump mb)])))

