(ns mjolnir.targets.nvptx-intrinsics
  (:require [mjolnir.expressions :as expr]
            [mjolnir.types :refer :all]
            [mjolnir.constructors-init :as const])
  (:alias c mjolnir.constructors))


(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.tid.x"} ^:extern TID_X [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.tid.y"} ^:extern TID_Y [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.tid.z"} ^:extern TID_Z [-> Int32])

(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ntid.x"} ^:extern NTID_X [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ntid.y"} ^:extern NTID_Y [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ntid.z"} ^:extern NTID_Z [-> Int32])

(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ctaid.x"} ^:extern CTAID_X [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ctaid.y"} ^:extern CTAID_Y [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.ctaid.z"} ^:extern CTAID_Z [-> Int32])

(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.nctaid.x"} ^:extern NCTAID_X [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.nctaid.y"} ^:extern NCTAID_Y [-> Int32])
(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.nctaid.z"} ^:extern NCTAID_Z [-> Int32])

(c/defn ^{:exact "llvm.nvvm.read.ptx.sreg.warpsize"} ^:extern WARPSIZE [-> Int32])



