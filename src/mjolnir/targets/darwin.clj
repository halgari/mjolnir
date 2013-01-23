(ns mjolnir.targets.darwin
  (:require [mjolnir.targets.target :as target]
            [clojure.java.shell :as shell]
            [mjolnir.types :as types]
            [mjolnir.targets.x86-cpus :as x86-cpus]
            [mjolnir.llvmc :as llvm]))


(defrecord DarwinTarget [march vendor os llvm-target]
  target/ITarget
  (pointer-type [this]
    (types/->IntegerType 32 #_(* (llvm/PointerSize (:target llvm-target)) 8)))
  (create-target-machine [this opts]
    (llvm/CreateTargetMachine (:target llvm-target)
                              (str march "-" vendor "-" os)
                              (or (:cpu opts) "generic")
                              (or (:features opts) "")
                              (or (target/code-gen-levels (:code-gen-level opts))
                                  llvm/LLVMCodeGenLevelDefault)
                              (or (target/reloc-modes (:reloc-mode opts))
                                  llvm/LLVMRelocDefault)
                              (or (target/code-models (:code-model opts))
                                  llvm/LLVMCodeModelDefault)))
  (emit-to-file [this module opts]
    (let [tm (target/create-target-machine this opts)
          err (llvm/new-pointer)
          file (or (:filename opts)
                   (llvm/temp-file "darwin_output" ""))]
      (when (llvm/TargetMachineEmitToFile tm
                                          module
                                          (name file)
                                          (or (target/output-types (:output-type opts))
                                              llvm/LLVMAssemblyFile)
                                          err)
        (assert false (.getString (llvm/value-at err) 0)))
      (llvm/DisposeMessage (llvm/value-at err))
      file)))

(defn get-march []
  (System/getProperty "os.arch"))

(defn get-vendor []
  "apple")

(defn get-os []
  (let [r (shell/sh "uname" "-r")]
    (str "darwin" r)))

(defn make-default-target []
  (let [t (case (get-march)
            "x86_64" "x86-64"
            "x86" "x86")]
    (map->DarwinTarget
     {:march (get-march)
      :vendor (get-vendor)
      :os (get-os)
      :llvm-target (target/find-llvm-target-by-name t)})))

(defn init-target [register-fn]
  (llvm/InitializeX86TargetInfo)
  (llvm/InitializeX86Target)
  (llvm/InitializeX86TargetMC)
  (llvm/InitializeX86AsmPrinter)
  (llvm/InitializeX86AsmParser)
  (register-fn make-default-target))
