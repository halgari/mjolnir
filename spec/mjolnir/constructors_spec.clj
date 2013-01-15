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
(defn pdebug [x]
  (fipp x)
  x)

(describe "constructors"
          (it "can create basic functions"
              (-> (c/module ['mjolnir.constructors-spec])
                  (pdebug)
                  (valid?)))
          (it "can compile created functions"
              (let [m (c/module ['mjolnir.constructors-spec])]
                  (pdebug m)
                  (valid? m)
                  (build m)))
          (it "can execute"
              (let [m (c/module ['mjolnir.constructors-spec])
                    _ (pdebug m)
                    _ (valid? m)
                    bm (build m)
                    e (engine bm)
                    f (get-fn e bm "mjolnir.constructors-spec/fib")]
                (println (time (f 40))))))