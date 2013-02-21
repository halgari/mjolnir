 (ns mjolnir.intrinsics
  (:require [mjolnir.expressions :as expr]
            [mjolnir.types :refer :all]
            [mjolnir.constructors-init :as const])
  (:alias c mjolnir.constructors))


(c/defn ^{:exact "llvm.sqrt.f64"} ^:extern llvm-sqrt-f64 [Float64 x -> Float64])
