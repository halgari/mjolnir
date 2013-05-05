(ns examples.mandelbrot
  (:require [criterium.core :as crit])
  (:require [mjolnir.constructors-init :refer [defnf]]
            [mjolnir.types :as types :refer [I8* Int64 Float32 Float32* Float64x4 Float64x4* VoidT]]
            [mjolnir.expressions :refer [build optimize dump ->ConstVector ->Do ->FPToSI ->SIToFP]]
            [mjolnir.config :as config]
            [mjolnir.targets.target :refer [emit-to-file as-dll]]
            [mjolnir.intrinsics :as intr]
            [mjolnir.targets.nvptx-intrinsics :refer [TID_X NTID_X CTAID_X TID_Y NTID_Y CTAID_Y]]
            [mjolnir.targets.nvptx :as nvptx]
            [mjolnir.core :refer [build-module]])
  (:alias c mjolnir.constructors)
  (:import [java.awt Color Image Dimension]
           [javax.swing JPanel JFrame SwingUtilities]
           [java.awt.image BufferedImage]
           [examples Mandelbrot]
           [com.sun.jna Native Pointer Memory]
           ))


(defn display-image [^floats d ^long width]
  (let [img (BufferedImage. width (/ (count d) width) BufferedImage/TYPE_INT_ARGB)
        height (long (/ (count d) width))]
    (println "Converting....")
    (dotimes [x width]
      (dotimes [y height]
        (let [idx (+ (* y width) x)
              val (unchecked-float (aget d idx))
              c (if (= val 1.0)
                  (Color/getHSBColor val 1 0)
                  (Color/getHSBColor (+ 0.5 val) 1 0.5))]
          (.setRGB img x y (.getRGB c)))))
    (doto
        (JFrame.)
      (.setContentPane
       (doto
           (proxy [JPanel]
               []
             (paintComponent [g]
               #_(proxy-super paintComponent g)
               (let [width (.getWidth this)
                     height (* width
                               (/ (.getHeight img nil)
                                  (.getWidth img nil)))
                     scaled (.getScaledInstance img width height Image/SCALE_SMOOTH)]
                 (.drawImage g scaled 0 0 this))))
         (.setPreferredSize (Dimension. 1024 1024))))
      (.setSize 1024 512)
      (.show))))

;; Mjolnir Method (no SSE)

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

(defnf ^:extern calc-mandelbrot-ptx [Float32* arr
                                     Float32 width
                                     Float32 height
                                     Float32 max
                                     -> VoidT]
  (let [xpx (cast Float32 (+ (* (CTAID_X) (NTID_X))
                             (TID_X)))
        ypx (cast Float32 (+ (* (CTAID_Y) (NTID_Y))
                             (TID_Y)))
        idx (cast Int64 (+ (* ypx width) xpx))
        c (calc-iteration xpx ypx max width height)]
    (aset arr idx (/ c max))))

(defn memory-to-array [^Memory m size]
  (let [arr (float-array size)]
    (dotimes [x size]
      (aset-float arr
                  x
                  (.getFloat m (* x 4))))
    arr))

(def WIDTH 1024.0)
(def HEIGHT 512.0)
(def SIZE (* WIDTH HEIGHT))

(defmulti run-command vector)

(defmethod run-command [:run :all]
  [_ _]
  (run-command :run :java))

(defmethod run-command [:run :java]
  [_ _]
  (let [img (time (Mandelbrot/calcMandelbrot (float-array SIZE)
                                             WIDTH
                                             1000))]
    (display-image img WIDTH)))


(defmethod run-command [:run :ptx]
  [_ _]
  (nvptx/init-target identity)
  (binding
      [config/*target* (nvptx/make-default-target)
       config/*float-type* Float32
       config/*int-type* Int64]
    (let [module (c/module ['examples.mandelbrot/square
                            'examples.mandelbrot/calc-iteration
                            'examples.mandelbrot/calc-mandelbrot-ptx
                            'mjolnir.targets.nvptx-intrinsics])
          built (optimize (build module))]
      (emit-to-file
       config/*target*
       built
       {:filename "mandelbrot2.ptx"
        :obj-type :asm})
      (let [dll (as-dll (nvptx/make-default-target) built {})
            f ((get dll calc-mandelbrot-ptx) [(/ WIDTH 16) (/ HEIGHT 32)] [16 32])
            ptr (nvptx/device-alloc (* SIZE 4))]
        (time (f ptr WIDTH HEIGHT 1000.0))
        (try
          (display-image (nvptx/to-float-array ptr SIZE) WIDTH)
          (finally
           (nvptx/free ptr)))
        (println "loaded")))))

(defmethod run-command [:run :mjolnir]
  [_ _]
  (binding [config/*target* (config/default-target)
                   config/*float-type* Float32
            config/*int-type* Int64]
    (let [module (c/module ['examples.mandelbrot/square
                            'examples.mandelbrot/calc-iteration
                            'examples.mandelbrot/calc-mandelbrot])
          built (optimize (build module))
          _ (dump built)
          dll (as-dll
               config/*target*
               built
               {:verbose true
                :obj-type :asm})
          mbf (get dll calc-mandelbrot)
          buf (Memory. (* SIZE 8))]
      (assert (and mbf dll) "Compilation error")
      (println "Running...")
      (time (mbf buf (float WIDTH) (float HEIGHT) (float 1000.0)))
      #_(println (distinct (memory-to-array buf SIZE)))
      (display-image (memory-to-array buf SIZE) WIDTH))))

(defmethod run-command [:benchmark :ptx]
  [_ _]
  (nvptx/init-target identity)
  (binding
      [config/*target* (nvptx/make-default-target)
       config/*float-type* Float32
       config/*int-type* Int64]
    (let [module (c/module ['examples.mandelbrot/square
                            'examples.mandelbrot/calc-iteration
                            'examples.mandelbrot/calc-mandelbrot-ptx
                            'mjolnir.targets.nvptx-intrinsics])
          built (optimize (build module))]
      (emit-to-file
       config/*target*
       built
       {:filename "mandelbrot2.ptx"
        :obj-type :asm})
      (let [dll (as-dll (nvptx/make-default-target) built {})
            f ((get dll calc-mandelbrot-ptx) #_[(/ WIDTH 128) (/ HEIGHT 64)] #_[128 64]
               [128 2] [8 256])
            ptr (nvptx/device-alloc (* SIZE 8))]
        (crit/quick-bench (f ptr WIDTH HEIGHT 1000.0))
        (nvptx/free ptr)))))


(defmethod run-command [:benchmark :mjolnir]
  [_ _]
  (binding [config/*target* (nvptx/make-default-target)
            config/*float-type* Float32
            config/*int-type* Int64]
    (let [module (c/module ['examples.mandelbrot/square
                            'examples.mandelbrot/calc-iteration
                            'examples.mandelbrot/calc-mandelbrot])
          built (build-module module)
          _ (dump built)
          dll (as-dll (config/default-target)
                      built
                      {:verbose true})
          mbf (get dll calc-mandelbrot)
          buf (Memory. (* SIZE 8))]
      (assert (and mbf dll) "Compilation error")
      (println "Running...")
      (crit/bench
       (mbf buf WIDTH HEIGHT 1000.0)))))

(defmethod run-command [:benchmark :java]
  [_ _]
  (crit/bench
   (Mandelbrot/calcMandelbrot (float-array SIZE)
                              WIDTH
                              1000)))

(defn -main [& opts]
  (apply run-command (map read-string opts)))



