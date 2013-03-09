(ns examples.vectors
  (:gen-class)
  (:require [criterium.core :as crit])
  (:require [mjolnir.constructors-init :as const]
            [mjolnir.types :as types :refer [I8* Int64 Float64 Float64x4 Float64x4*]]
            [mjolnir.expressions :refer [build optimize dump ->ConstVector]]
            [mjolnir.config :as config]
            [mjolnir.targets.target :refer [emit-to-file as-dll]]
            [mjolnir.intrinsics :as intr])
  (:alias c mjolnir.constructors)
  (:import [examples Vectors]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(definterface vec-interface
  (^doubles gen_vector [^long size])
  (^doubles normalize [^doubles arr ^long size]))



(defn clj-square ^double [^double x]
  (* x x))

(defn clj-vector-length ^double [^doubles v ^long size]
  (loop [sum 0.0
         idx 0]
    (if (< idx size)
      (recur (-> (aget v idx)
                 clj-square
                 (+ sum))
             (inc idx))
      (Math/sqrt sum))))

(def ^vec-interface clj-imp
  (reify
    vec-interface
    (^doubles gen_vector [this ^long size]
      (double-array (map double (range size))))

    (^doubles normalize [this ^doubles v ^long size]
      (let [len (clj-vector-length v size)]
        (dotimes [idx size]
          (aset-double v idx (/ (aget v idx) len)))))))



(defn clj-gen-vector ^doubles [^long size]
  (double-array (map double (range size))))


;;; Mjolnir implementation
#_(set! config/default-float-type Float64)
#_(set! config/default-int-type Int64)

(def I8** (types/->PointerType I8*))

(c/defn ^:extern ^:exact posix_memalign [I8** ptr Int64 alignment Int64 size -> Int64])

(c/defn ^:extern mj-create-buffer [Int64 size -> I8*]
  (c/using [x (c/malloc I8* 1)]
         (posix_memalign (c/bitcast x I8**)
                         64
                         size)
         (c/aget x 0)))


(c/defn mj-square [Float64x4 x -> Float64x4]
  (c/* x x))

(c/defn mj-hadd [Float64x4 v -> Float64]
  (c/+ (c/eget v 0)
       (c/eget v 1)
       (c/eget v 2)
       (c/eget v 3)))

(c/defn mj-propagate [Float64 v -> Float64x4]
  (-> (c/const [0.0 0.0 0.0 0.0] -> Float64x4)
      (c/eset 0 v)
      (c/eset 1 v)
      (c/eset 2 v)
      (c/eset 3 v)))

(c/defn mj-length [Float64x4* v Int64 size -> Float64]
  (c/loop [sum (c/const [0.0 0.0 0.0 0.0] -> Float64x4)
           idx 0]
          (c/if (c/< idx size)
                (c/recur (-> (c/aget v idx)
                             mj-square
                             (c/+ sum))
                         (c/+ idx 1)
                         -> Float64)
                (intr/llvm-sqrt-f64 (mj-hadd sum)))))

(c/defn ^:export mj-normalize [Float64x4* v Int64 size -> Float64x4*]
  (c/let [len (mj-length v size)
          lenv (mj-propagate len)]
         (c/loop [idx 0]
                 (c/if (c/< idx size)
                       (c/do (c/aset v idx
                                     (-> (c/aget v idx)
                                         (c/fdiv lenv)))
                             (c/recur (c/+ idx 1)
                                      -> Float64x4*))
                       v))))
(def mjolnir-dll
  (config/with-config [Int64 Float64 (config/default-target)]
    (let [m (c/module ['examples.vectors
                       'mjolnir.intrinsics/llvm-sqrt-f64])
          built (optimize (build m))
          bf (emit-to-file config/*target*
                           built {:filename "vectors2.s"
                                  :output-type :asm
                                  :cpu :core-avx-i})
          _ (dump built)
          dll (as-dll config/*target*
                      built
                      {:verbose true
                       :cpu :core-avx-i})]
      dll)))

(def fn-create-buffer (get mjolnir-dll mj-create-buffer))
(def fn-mj-normalize (get mjolnir-dll mj-normalize))


(comment

;;; ptx code

  (c/defn ptx-length [Float64* v Int64 size -> Float64]
    (c/loop [sum (c/const 0 Float64)
             idx 0]
            (c/if (c/< idx size)
                  (c/recur
                   (c/+ sum
                        (c/aget v (c/+ (c/* (ptx/threaddim_x)
                                            idx)
                                       (ptx/threadidx_x)))))
                  sum -> Float64)))

  (c/defn ptx-normalize [Float64* v Float64* gbl Int64 size -> Float64*]
    (ptx/atomic-add gbl (ptx-length v size))
    (ptx/sync-threads)
    (c/let [s (ptx/sqrt (aget gbl 0))]
           )))

(def ^long size (* 1024 1024 16))


(defn -main []
  (let [vec (.gen-vector clj-imp size)]
    (println "Testing CLJ Implementation...")
    (crit/quick-bench
     (do (.normalize clj-imp vec size))))
  (let [vec (Vectors/createBuffer size)]
    (println "Testing Java Implementation...")
    (crit/quick-bench
     (Vectors/normalize vec size)))
  (let [vec (fn-create-buffer (* 16 1024 1024 8))]
    (println "Testing Mjolnir (SSE) Code...")
    (crit/quick-bench
     (fn-mj-normalize vec (/ (* 16 1024 1024) 4)))))
