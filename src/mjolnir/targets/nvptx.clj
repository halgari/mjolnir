(ns mjolnir.targets.nvptx
  (:require
   [mjolnir.expressions :as expr]
   [mjolnir.targets.target :as target]
            [clojure.java.shell :as shell]
            [mjolnir.types :as types]
            [mjolnir.targets.nvptx-cpus :as nvptx-cpus]
            [mjolnir.llvmc :as llvm :refer [defnative]])
  
  (:import [jcuda Pointer NativePointerObject]
           [jcuda.driver CUmodule JCudaDriver CUfunction CUdeviceptr CUdevice CUcontext]))

(def allowed-char? (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_123456790"))

(defn convert-to-char [x]
  (if (allowed-char? x)
    x
    (str "_" (.toUpperCase (Integer/toHexString (int x))) "_")))

(defn mangle-ptx [nm]
  (apply str (map convert-to-char nm)))

(defn to-ptx-arg [a]
  (cond
   (float? a) (Pointer/to (float-array [a]))
   :else (Pointer/to (into-array NativePointerObject [a]))))

(defn encode-args [args]
  (Pointer/to
   (into-array NativePointerObject
               (map
                to-ptx-arg
                args))))


(defn free [ptr]
  (JCudaDriver/cuMemFree ptr))

(defn to-float-array ^floats [ptr size]
  (let [arr (float-array size)]
    (JCudaDriver/cuMemcpyDtoH (Pointer/to arr)
                              ptr
                              (* size 4))
    arr))

(defn to-float-array! [ptr size]
  (let [arr (to-float-array ptr size)]
    (free ptr)
    arr))

(defn device-alloc [size]
  (let [ptr (CUdeviceptr.)]
    (JCudaDriver/cuMemAlloc ptr size)
    ptr))

(defmacro cu [nm & args]
  (let [fname (symbol "JCudaDriver" (str "cu" nm))]
    `(let [r# (~fname ~@args)]
       (assert (= r# 0) (str "Cuda Call error " ~(name nm) " " r#))
       r#)))


(defrecord NVPTXTarget64 [march llvm-target]
  target/ITarget
  (pointer-type [this]
    (types/->IntegerType 64))
  (default-address-space [this]
    1)
  (get-calling-conv [this extern?]
    (if extern?
      llvm/PTXGlobal
      llvm/PTXDevice))
  (create-target-machine [this opts]
    (llvm/CreateTargetMachine (:target llvm-target)
                              "nvptx64-generic-generic"
                              (or (nvptx-cpus/cpus (:cpu opts)) "sm_30")
                              (or (:features opts) "sm_30")
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
                   (llvm/temp-file "nvptx_output" (case (:output-type opts)
                                                     :asm ".s"
                                                     :obj ".ptx"
                                                     ".ptx")))]
      (when (llvm/TargetMachineEmitToFile tm
                                          module
                                          (name file)
                                          (or (target/output-types (:output-type opts))
                                              llvm/LLVMAssemblyFile)
                                          err)
        (assert false (.getString (llvm/value-at err) 0)))
      (llvm/DisposeMessage (llvm/value-at err))
      file))
  (as-dll [this module opts]
    (let [tm (target/create-target-machine this opts)
          err (llvm/new-pointer)
          file (or (:filename opts)
                   (llvm/temp-file "nvptx_output" (case (:output-type opts)
                                                     :asm ".s"
                                                     :obj ".ptx"
                                                     ".ptx")))
          cumodule (CUmodule.)]
      (when (llvm/TargetMachineEmitToFile tm
                                          module
                                          (name file)
                                          (or (target/output-types (:output-type opts))
                                              llvm/LLVMAssemblyFile)
                                          err)
        (assert false (.getString (llvm/value-at err) 0)))
      (llvm/DisposeMessage (llvm/value-at err))
      (cu ModuleLoad cumodule file)
      (reify
        clojure.lang.ILookup
        (valAt [this k]
          (.valAt this k nil))
        (valAt [this k el]
          (let [function (CUfunction.)
                nm (-> (k) :fn)]
            (println "finding" (mangle-ptx (:name nm)))
            (cu ModuleGetFunction function cumodule (mangle-ptx (:name nm)))
            (fn [[bx by bz] [gx gy gz]]
              (fn [& args]
                (let [args (encode-args args)]
                  (cu LaunchKernel
                      function
                      (or gx 1)
                      (or gy 1)
                      (or gz 1)
                      (or bx 1)
                      (or by 1)
                      (or bz 1)
                      0 nil
                      args nil)
                  (cu CtxSynchronize)
                  args)))))))))


(defnative Integer LLVMInitializeNVPTXTargetInfo)
(defnative Integer LLVMInitializeNVPTXTarget)
(defnative Integer LLVMInitializeNVPTXTargetMC)
(defnative Integer LLVMInitializeNVPTXAsmPrinter)

(defn make-default-target []
  (map->NVPTXTarget64
   {:march "nvptx64"
    :llvm-target (target/find-llvm-target-by-name "nvptx64")}))

(defn init-target [register-fn]
  (JCudaDriver/setExceptionsEnabled true)
  (cu Init 0)
  (let [a (int-array [0])]
    (cu DeviceGetCount a)
    (println "Number Of Devices: " (aget a 0) ))
  (let [device (CUdevice.)
        ctx (CUcontext.)]
    (JCudaDriver/cuDeviceGet device 0)
    (JCudaDriver/cuCtxCreate ctx 0 device))
  (InitializeNVPTXTargetInfo)
  (InitializeNVPTXTarget)
  (InitializeNVPTXTargetMC)
  (InitializeNVPTXAsmPrinter)
  (register-fn make-default-target))