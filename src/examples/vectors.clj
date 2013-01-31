(ns examples.vectors
  (:require [criterium.core :as crit]
            [mjolnir.constructors-init :as const]
            [mjolnir.types :as types :refer [I8* Int32 Float32 Float32x4]]
            [mjolnir.expressions :refer [build pdebug optimize dump ->ConstVector]]
            [mjolnir.config :as config]
            [mjolnir.targets.target :refer [emit-to-file as-dll]])
  (:alias c mjolnir.constructors)
  (:import [javax.imageio ImageIO]
           [java.io File]
           [java.awt.image BufferedImage]))

(def floats (class (float-array 1)))

(def raw-img (ImageIO/read (File. "/Users/tim/Downloads/IMAG0027.jpg")))


(defn get-offset ^long [^long size-x ^long x ^long y ^long el]
  (-> y (* size-x) (+ (* 4 x) el)))

(defn tof ^double [^long l]
  (/ (double l) 256))

(defn get-floats [^BufferedImage img]
  (let [arr (float-array (* 4 (.getHeight img) (.getWidth img)))]
    (dotimes [y (.getHeight img)]
      (dotimes [x (.getWidth img)]
        (let [argb (.getRGB img x y)
              a (bit-and (bit-shift-right argb 24) 0xFF)
              r (bit-and (bit-shift-right argb 16) 0xFF)
              g (bit-and (bit-shift-right argb 8) 0xFF)
              b (bit-and (bit-shift-right argb 0) 0xFF)]
          (aset-float arr (get-offset (.getWidth img) x y 0) (tof a))
          (aset-float arr (get-offset (.getWidth img) x y 1) (tof r))
          (aset-float arr (get-offset (.getWidth img) x y 2) (tof g))
          (aset-float arr (get-offset (.getWidth img) x y 3) (tof b)))))
    arr))

(def ^floats img [] #_(time (get-floats raw-img)))

(defn get-smooth-pixel ^double [^long size-x ^long x ^long y ^long el]
  (-> (+ (* 0.1 (aget img (get-offset size-x (dec x) (dec y) el)))
         (* 0.1 (aget img (get-offset size-x x (dec y) el))
         (* 0.1 (aget img (get-offset size-x (inc x) (dec y) el)))

         (* 0.1 (aget img (get-offset size-x (dec x) y el)))
         (* 0.2 (aget img (get-offset size-x x y el)))
         (* 0.1 (aget img (get-offset size-x (inc x) y el)))
         
         (* 0.1 (aget img (get-offset size-x (dec x) (inc y) el)))
         (* 0.1 (aget img (get-offset size-x x (inc y) el)))
         (* 0.1 (aget img (get-offset size-x (inc x) (inc y) el)))))
      (/ 10)
      double))

(defn smooth-img [img size-y size-x]
  (let [out (float-array (count img))]
    (dotimes [y (- size-y 2)]
      (dotimes [x (- size-x 2)]
        (dotimes [el 4]
          (->> (get-smooth-pixel size-x (inc x) (inc y) el)
               float
               (aset-float img (get-offset size-y (inc x) (inc y) el))))))))


;;;; LLVM code ;;;

(def I8** (types/->PointerType I8*))
(def Float32* (types/->PointerType Float32))
(def Float32x4* (types/->PointerType Float32x4))

(c/defn ^:extern ^:exact posix_memalign [I8** ptr Int32 alignment Int32 size -> Int32])

(c/defn ^:extern mj-create-buffer [Int32 width Int32 height -> I8*]
  (c/using [x (c/malloc I8* 1)]
         (posix_memalign (c/bitcast x I8**)
                         16
                         (c/* width height 4 4))
         (c/aget x 0)))

(c/defn mj-get-offset [Int32 size-x Int32 x Int32 y Int32 el -> Int32]
  (-> y (c/* size-x) (c/+ (c/* 4 x) el)))

(c/defn mjv-get-offset [Int32 size-x Int32 x Int32 y -> Int32]
  (c/+ (c/* size-x y) x))

#_(c/defn ^:extern mj-set-pixel [I8* img
                             Int32 width
                             Int32 height
                             Int32 x
                             Int32 y
                             Int32 el
                             Float32 val
                             -> I8*]
  (c/if (c/and (c/< x width)
               (c/>= x 0)
               (c/< y height)
               (c/>= y 0))
        (c/aset (c/bitcast img Float32*)
                (mj-get-offset height x y el)
                val)
        (float 0.0)))


#_(c/defn ^:extern mj-get-pixel [I8* img
                             Int32 width
                             Int32 height
                             Int32 x
                             Int32 y
                             Int32 el
                             Float32 val
                             -> Float32]
  (c/if (c/and (c/< x width)
               (c/>= x 0)
               (c/< y height)
               (c/>= y 0))
        (c/aget (c/bitcast img Float32*)
                (mj-get-offset height x y el))
        (float 0.0)))

(def v1 (->ConstVector [0.1 0.1 0.1 0.1]))
(def v2 (->ConstVector [0.2 0.2 0.2 0.2]))

(c/defn mj-get-smooth-pixel [Float32* img
                          Int32 size-x
                          Int32 size-y
                          Int32 x
                          Int32 y
                          Int32 el
                          -> Float32]
  (-> (c/+ (c/* 0.1 (c/aget img (mj-get-offset size-x (c/dec x) (c/dec y) el)))
           (c/* 0.1 (c/aget img (mj-get-offset size-x x (c/dec y) el)))
           (c/* 0.1 (c/aget img (mj-get-offset size-x (c/inc x) (c/dec y) el)))

           (c/* 0.1 (c/aget img (mj-get-offset size-x (c/dec x) y el)))
           (c/* 0.2 (c/aget img (mj-get-offset size-x x y el)))
           (c/* 0.1 (c/aget img (mj-get-offset size-x (c/inc x) y el)))
           
           (c/* 0.1 (c/aget img (mj-get-offset size-x (c/dec x) (c/inc y) el)))
           (c/* 0.1 (c/aget img (mj-get-offset size-x x (c/inc y) el)))
           (c/* 0.1 (c/aget img (mj-get-offset size-x (c/inc x) (c/inc y) el))))
      (c/* (double 0.1))))

(c/defn mjv-get-smooth-pixel [Float32x4* img
                          Int32 size-x
                          Int32 size-y
                          Int32 x
                          Int32 y
                          -> Float32x4]
  (c/+ (c/* v1 (c/aget img (mjv-get-offset size-x (c/dec x) (c/dec y))))
          (c/* v1 (c/aget img (mjv-get-offset size-x x (c/dec y))))
          (c/* v1 (c/aget img (mjv-get-offset size-x (c/inc x) (c/dec y))))

          (c/* v1 (c/aget img (mjv-get-offset size-x (c/dec x) y)))
          (c/* v2 (c/aget img (mjv-get-offset size-x x y)))
          (c/* v1 (c/aget img (mjv-get-offset size-x (c/inc x) y)))
          
          (c/* v1 (c/aget img (mjv-get-offset size-x (c/dec x) (c/inc y))))
          (c/* v1 (c/aget img (mjv-get-offset size-x x (c/inc y))))
          (c/* v1 (c/aget img (mjv-get-offset size-x (c/inc x) (c/inc y))))))


(c/defn mj-smooth-img [Float32* img Int32 size-y Int32 size-x -> Float32*]
  (c/let [out (c/bitcast (mj-create-buffer size-x size-y)
                         Float32*)]
         (c/dotimes [y (c/+ size-y -2)]
                    (c/dotimes [x (c/+ size-x -2)]
                               (c/dotimes [el 4]
                                          (->> (mj-get-smooth-pixel img size-x size-y (c/inc x) (c/inc y) el)
                                               (c/aset out (mj-get-offset size-x (c/inc x) (c/inc y) el))))))
         out))

(c/defn mjv-smooth-img [Float32x4* img Int32 size-y Int32 size-x -> Float32x4*]
  (c/let [out (c/bitcast (mj-create-buffer size-x size-y)
                         Float32x4*)]
         (c/dotimes [y (c/+ size-y -2)]
                    (c/dotimes [x (c/+ size-x -2)]
                               (->> (mjv-get-smooth-pixel img size-x size-y (c/inc x) (c/inc y))
                                    (c/aset out (mjv-get-offset size-x (c/inc x) (c/inc y))))))
         out))




(defn mj-tests []
  (let [m (c/module ['examples.vectors])
        #_(pdebug m)
        _ (Thread/sleep 1000)
        built (build m)]
    (let [target (config/default-target)
          bf (emit-to-file target (optimize built) {:filename "foo.s" :output-type :asm})
          dll (as-dll target
                      (optimize built)
                      {:verbose true})
          create-buffer (get dll mj-create-buffer)
          buf (create-buffer 3265 1952)
          smooth (get dll mj-smooth-img)
          smoothv (get dll mjv-smooth-img)]
      (println "Testing 'Normal' Mjolnir code")
      (crit/quick-bench (smooth buf 3264 1952))
      (println "done")
      
      (println "Testing SSE Enhanced Mjolnir code")
      (crit/quick-bench (smoothv buf 3264 1952))
      (println "done"))))


(defn -main []
  (mj-tests)
  (comment
    (let [width (.getWidth raw-img)
          height (.getHeight raw-img)]
      (time (smooth-img img height width)))
    (println (count img))))

(comment
  (def image-size 1024)

  (c-defn get-poffset [SizeT x SizeT y -> SizeT]
          (c/iadd (c/imul y image-size)))

  (c-defn get-pixelv [float32x4* image SizeT x SizeT y -> float32x4]
          (c/if (c/and (c/>= x 0)
                       (c/< x image-size)
                       (c/>= y 0)
                       (c/> y image-size))
                (c/aget image (get-offset x y))
                (c/const float32x4 [0.0 0.0 0.0 0.0])))


  (c-defn get-smooth-pixelv [float32x4* image SizeT x SizeT y -> float32x4]
          (c/fdiv (c/fadd (get-pixelv x (c/iadd y -1))
                          (get-pixelv (c/iadd x -1) (c/iadd y -1))
                          (get-pixelv (c/iadd x 1) (c/iadd y -1))

                          (get-pixelv x y)
                          (get-pixelv (c/iadd x -1) y)
                          (get-pixelv (c/iadd x 1) y)

                          (get-pixelv x (c/iadd y 1))
                          (get-pixelv (c/iadd x -1) (c/iadd y 1))
                          (get-pixelv (c/iadd x 1) (c/iadd y 1)))
                  (c/const float32x4 [9.0 9.0 9.0 9.0]))))