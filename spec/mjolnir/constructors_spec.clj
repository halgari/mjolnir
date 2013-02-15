(ns mjolnir.constructors-spec
  (:require [speclj.core :refer :all]
            [mjolnir.types :refer :all]
            [mjolnir.expressions :refer :all]
            [mjolnir.constructors-init :as cinit]
            [mjolnir.transforms.ref-count :as rc]
            [clojure.pprint :refer [pprint]]
            )
  (:alias c mjolnir.constructors))


 

(c/defn fib [Int64 x -> Int64]
                (c/if (c/or (c/is x 0)
                            (c/is x 1))
                      x
                      (c/iadd (fib (c/isub x 1))
                              (fib (c/isub x 2)))))

(def buf-size (* 1024 1024))

(c/defn cnt [Int64 x -> Int64]
  (c/using [frame (c/malloc Int64 buf-size)]
           #_(c/dotimes [y buf-size]
                      (c/aset frame y 1))
           #_(c/loop [i 0
                    idx 0]
                   (c/if (c/is idx buf-size)
                         i
                         (c/recur (c/iadd i (c/aget frame idx))
                                  (c/iadd idx 1)
                                  -> Int64)))))


;; Tests basic recursion
(c/defn cnt-to [Int64 final -> Int64]
  (c/loop [i 0]
          (c/if (c/is i final)
                i
                (c/recur (c/iadd i 1) -> Int64))))

;; Tests ASet/AGet
(c/defn aset-aget [Int64 num -> Int64]
  (c/using [data (c/malloc Int64 5)]
           (c/aget (c/aset data 0 num) 0)))

;; Tests to see if we can nex
(c/defn nested-dotimes [-> Int64]
  (c/dotimes [x 100]
             (c/dotimes [y 200]
                        (c/+ x y)
      #_(c/dotimes [z 300]
        (c/+ x y z)))))

(c/defstruct parent-struct
  :members [Int64 v1])

(c/defstruct child-struct
  :extends parent-struct
  :members [Int64 v2])

(c/defstruct grandchild-struct
  :extends child-struct
  :members [Int64 v3])

;; Tests basic struct get-set



(c/defn structs-test [Int64 num -> Int64]
  (c/using [res (c/bitcast (c/malloc grandchild-struct 1)
                           (->PointerType grandchild-struct))]
           (c/set res :v2 num)
           (-v2 res)))

(c/def fourty-two 42 -> Int64)

(c/def fourty-two-p fourty-two -> Int64*)

(c/defn get-global [-> Int64]
  (c/aget fourty-two-p 0))


;; Tests of ref counting

(def Int64TypeID 1)

(c/defstruct WObject
  :members [Int64 type
            Int64 refcnt]
  :gc {:type :ref-count
       :inc ::object-incref
       :dec ::object-decref})

(def Object* (->PointerType WObject))

(c/defn object-incref [Object* o -> Object*]
  (c/set o :refcnt (c/+ (-refcnt o) 1)))

(c/defn object-decref [Object* o -> Int64]
  (c/let [nc (c/+ (-refcnt o) -1)]
         (c/if (c/is nc 0)
               (do (c/free o)
                   o)
               (c/set o :refcnt nc))
         nc))

(c/defstruct WInt64
  :extends WObject
  :members [Int64 int-val])

(def WInt64* (->PointerType WInt64))

(c/defn wrap-Int64 [Int64 val -> Object*]
  (c/bitcast (c/new WInt64 Int64TypeID 1 val)
             Object*))

(c/defn unwrap-Int64 [Object* o -> Int64]
  (-int-val (c/bitcast o WInt64*)))

(c/defn Int64-Add [Object* a Object* b -> Object*]
  (-> (c/+ (unwrap-Int64 a)
           (unwrap-Int64 b))
      wrap-Int64))

(c/defn test-refc [Int64 val -> Int64]
  (-> (Int64-Add (wrap-Int64 val)
                 (wrap-Int64 val))
      unwrap-Int64))

;;;

(defn idbg [x]
  (pprint x)
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
                    #_(dump mb)]))
          (it "can create and access structs"
              (let [m (c/module ['mjolnir.constructors-spec/structs-test])
                    _ (valid? m)
                    mb (build m)
                    #_(dump mb)
                    ]))
          (it "can create and access globals"
              (let [m (c/module ['mjolnir.constructors-spec/fourty-two
                                 'mjolnir.constructors-spec/fourty-two-p
                                 'mjolnir.constructors-spec/get-global])
                    _ (valid? m)
                    mb (build m)
                    #_(dump mb)
                    ]))
          (it "supports refcounting"
              (let [m (c/module ['mjolnir.constructors-spec/object-incref
                                 'mjolnir.constructors-spec/object-decref
                                 'mjolnir.constructors-spec/wrap-Int64
                                 'mjolnir.constructors-spec/unwrap-Int64
                                 'mjolnir.constructors-spec/Int64-Add
                                 'mjolnir.constructors-spec/test-refc])]
                (should-not= [] 
                             (idbg (rc/query-refcnt-nodes m)))
                #_mb #_(optimize (build m))
                #__ #_(dump mb))))

