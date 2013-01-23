(ns examples.vectors)

(def image-size 1024)

(c-defn get-poffset [SizeT x SizeT y -> SizeT]
        (c/iadd (c/imul y image-size)))

(c-defn get-pixelv [Float32x4* image SizeT x SizeT y -> Float32x4]
        (c/if (c/and (c/>= x 0)
                     (c/< x image-size)
                     (c/>= y 0)
                     (c/> y image-size))
              (c/aget image (get-offset x y))
              (c/const Float32x4 [0.0 0.0 0.0 0.0])))


(c-defn get-smooth-pixelv [Float32x4* image SizeT x SizeT y -> Float32x4]
        (c/fdiv (c/fadd (get-pixelv x (c/iadd y -1))
                        (get-pixelv (c/iadd x -1) (c/iadd y -1))
                        (get-pixelv (c/iadd x 1) (c/iadd y -1))

                        (get-pixelv x y)
                        (get-pixelv (c/iadd x -1) y)
                        (get-pixelv (c/iadd x 1) y)

                        (get-pixelv x (c/iadd y 1))
                        (get-pixelv (c/iadd x -1) (c/iadd y 1))
                        (get-pixelv (c/iadd x 1) (c/iadd y 1)))
                (c/const Float32x4 [9.0 9.0 9.0 9.0])))