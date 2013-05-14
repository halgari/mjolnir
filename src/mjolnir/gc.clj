(ns mjolnir.gc)

(defprotocol GC
  (build-new [this d module builder fn inst defs])
  (add-globals [this conn]))