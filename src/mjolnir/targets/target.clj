(ns mjolnir.targets.target
  (:require [mjolnir.llvmc :as llvm]))

;; The purpose of these protocols is to provide an abstract interface
;; by which a user can locate machine/os specific information and then
;; use that information to compile/assemble code



(defprotocol ITarget
  (pointer-type [this] "Get the system pointer mjolnir type")
  (create-target-machine [this opts] "Creates an llvm target machine from this target")
  (emit-to-file [this module opts] "Writes the module to a file with the specified options")
  (as-dll [this module opts] "Compiles the module as a shared library"))

(defn find-llvm-target-by-name [name]
  (println (llvm/target-seq))
  (Thread/sleep 1000)
  (first (filter (comp (partial = name) :name)
                 (llvm/target-seq))))


(def code-gen-levels
  {:none llvm/LLVMCodeGenLevelNone
   :aggressive llvm/LLVMCodeGenLevelAggressive})

(def reloc-modes {})

(def code-models {})

(def output-types
  {:asm llvm/LLVMAssemblyFile
   :obj llvm/LLVMObjectFile})




