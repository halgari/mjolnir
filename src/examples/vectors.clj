(ns examples.vectors
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

(def ^floats img (time (get-floats raw-img)))

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

(defn -main []
  (let [width (.getWidth raw-img)
        height (.getHeight raw-img)]
    (time (smooth-img img height width)))
  (println (count img)))

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