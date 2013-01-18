(ns mjolnir.constructors-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.constructors-init :as cinit]
            [bbloom.fipp.edn :refer (pprint pretty pretty-map) :rename {pprint fipp}])
  (:alias c mjolnir.constructors))

(defmethod pretty clojure.lang.IRecord [r]
    [:span (-> r class .getName (clojure.string/split #"\.") last) (pretty-map r)])

 

(c/defn fib [Int32 x -> Int32]
                (c/if (c/or (c/is x 0)
                            (c/is x 1))
                      x
                      (c/iadd (fib (c/isub x 1))
                              (fib (c/isub x 2)))))

(def buf-size (* 1024 1024))

(c/defn cnt [Int32 x -> Int32]
  (c/using [frame (c/malloc Float32 buf-size)]
           (c/dotimes [y buf-size]
                      (c/aset frame y 1.0))
           #_(c/loop [i 0.0
                    idx 0]
                   (c/if (c/is idx buf-size)
                         i
                         (c/recur (c/fadd i (c/aget frame idx))
                                  (c/iadd idx 1))))))


;; Tests basic recursion
(c/defn cnt-to [Int32 final -> Int32]
  (c/loop [i 0]
          (c/if (c/is i final)
                i
                (c/recur (c/iadd i 1) -> Int32))))

(defn pdebug [x]
  (fipp x)
  x)

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
          (it "can run count function"
              (let [m (c/module ['mjolnir.constructors-spec/cnt-to])
                    _ (pdebug m)
                    _ (valid? m)
                    bm (build m)])))