(ns mjolnir.types
  (:require [mjolnir.llvmc :as llvm]
            [mjolnir.config :refer :all]
            [mjolnir.ssa :refer :all]
            [mjolnir.targets.target :refer :all])
  (:import [com.sun.jna Native Pointer]))


(defmacro assure [pred]
  `(assert ~pred (str "at: " (pr-str (meta (:location ~'this)))
                      " got: " (pr-str ~(second pred)))))


(defmacro validate-all [& body]
  `(apply valid? ~(vec body)))

#_(defmacro assure-same-type [& body]
  `(reduce (fn [a# x#]
             (assert (= a# x#)
                     (str "Expected same types, "
                      "at: " (pr-str (meta (:location ~'this)))
                      " got: " (pr-str ~(vec body)))))e
                      ~(vec body)))

(defmacro assure-same-type [& args]
  (identity 1))

(defprotocol Validatable
  (validate [this]))

(defprotocol Type
  (llvm-type [this]))


(defn valid? [tp]
  (validate tp)
  true)

(defprotocol ElementPointer
  (etype [this]))

(defprotocol ConstEncoder
  (encode-const [this val] "Encodes the value as a const with this type"))

(defn ElementPointer? [t]
  (extends? ElementPointer (type t)))

(defn type? [this]
  (extends? Type (type this)))

(defn assure-type [tp]
  (assert (and (extends? Type (type tp))
               (valid? tp))
          (str "at: " (pr-str (meta (:location tp)))
                      " got: " (pr-str tp))))

(defrecord IntegerType [width]
  Validatable
  (validate [this]
    (assure (integer? width)))
  Type
  (llvm-type [this]
    (llvm/IntType width))
  ConstEncoder
  (encode-const [this val]
    (llvm/ConstInt (llvm-type this) val true))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [this-id (singleton
               {:node/type :type/int
                :type/width width}
               this)]
     this-id)))


(defrecord VoidType []
  Validatable
  (validate [this]
            true)
  Type
  (llvm-type [this]
    (llvm/VoidType))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [this-id (singleton
               {:node/type :type/void})]
     this-id)))

(defn integer-type? [tp]
  (instance? IntegerType tp))

(defrecord FloatType [width]
  Validatable
  (validate [this]
    (assure (integer? width)))
  Type
  (llvm-type [this]
    (case width
      32 (llvm/FloatType)
      64 (llvm/DoubleType)))
  ConstEncoder
  (encode-const [this val]
    (llvm/ConstReal (llvm-type this) val))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [id (singleton
          {:node/type :type.float
           :type/width width}
          this)]
     id)))

(defn float-type? [tp]
  (instance? FloatType tp))

(declare const-string-array)


(defrecord PointerType [etype]
  Validatable
  (validate [this]
    
    (assure-type etype))
  Type
  (llvm-type [this]
    (llvm/PointerType (llvm-type etype)
                      (default-address-space *target*)))
  ElementPointer
  (etype [this]
    (:etype this))
  ConstEncoder
  (encode-const [this val]
    (if (nil? val)
      (llvm/ConstNull (llvm-type this))
      (if (and (= etype (->IntegerType 8))
               (string? val))
        (const-string-array val)
        (let [nm (:name val)
              ng (llvm/GetNamedGlobal *module* nm)]
          (assert ng (str "Could not find global: " nm))
          ng))))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [tp (add-to-plan etype)
      this-id (singleton {:node/type :type/pointer
                          :type/element-type tp}
                         this)]
     this-id)))




(defn pointer-type? [tp]
  (instance? PointerType tp))

(defrecord VectorType [etype length]
  Validatable
  (validate [this]
    (assure-type etype)
    (assure (integer? length)))
  Type
  (llvm-type [this]
    (llvm/VectorType (llvm-type etype) length))
  ElementPointer
  (etype [this]
    (:etype this))
  ConstEncoder
  (encode-const [this val]
    (assert (= length (count val)) "Const must be the same length as the vector")
    (llvm/ConstVector (llvm/map-parr (partial encode-const etype) val)
                      (count val))))

(defn vector-type? [tp]
  (instance? VectorType tp))

(defrecord ArrayType [etype cnt]
  Validatable
  (validate [this]
    (assure-type etype)
    (assure (integer? cnt)))
  Type
  (llvm-type [this]
    (llvm/ArrayType (llvm-type etype) cnt))
  ElementPointer
  (etype [this]
    (:etype this))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [tp (add-to-plan etype)
      this-id (singleton {:node/type :type/array
                          :type/element-type tp
                          :type/length cnt}
                         this)]
     this-id)))

(defrecord FunctionType [arg-types ret-type]
  Validatable
  (validate [this]
    (every? assure-type arg-types)
    (assure-type ret-type))
  Type
  (llvm-type [this]
    (llvm/FunctionType (llvm-type ret-type)
                       (llvm/map-parr llvm-type arg-types)
                       (count arg-types)
                       false))
  IToPlan
  (add-to-plan [this]
    (gen-plan
     [args (add-all (map add-to-plan arg-types))
      ret (add-to-plan ret-type)
      seq (assert-seq args)
      this-id (singleton (merge {:node/type :type/fn
                                 :type.fn/return ret}
                                (when seq
                                  {:type.fn/arguments seq})))
      _ (assert-all (map (fn [x idx]
                           [{:fn.arg/type x
                             :fn.arg/idx idx
                             :fn.arg/fn this-id}
                            nil])
                      args
                      (range)))]
     this-id)))


(defn flatten-struct [tp]
  (->> (take-while (complement nil?)
                   (iterate :extends tp))
       reverse
       (mapcat :members)))

(defn seq-idx [col ksel k]
  (-> (zipmap (map ksel col)
              (range))
      (get k)))

(defn member-idx [struct member]
  (-> (flatten-struct struct)
      (seq-idx second member)))



(defrecord StructType [name extends members]
  Validatable
  (validate [this]
    (when extends
      (assure (instance? StructType extends)))
    (doseq [[tp name] members]
      (assure (keyword? name))
      (assure (extends? Type (class tp)))))
  Type
  (llvm-type [this]
    (let [mems (flatten-struct this)]
      (llvm/StructType (llvm/map-parr (comp llvm-type first)
                                      mems)
                       (count mems)
                       true))))

(defn StructType? [tp]
  (instance? StructType tp))

(defn FunctionType? [tp]
  (instance? FunctionType tp))


(defn const-string-array [s]
  (let [ar (into-array Pointer (map #(llvm/ConstInt (llvm-type (->IntegerType 8)) % false)
                                    (concat s [0])))
        llvm-ar (llvm/ConstArray (llvm-type (->IntegerType 8))
                        ar
                        (count ar))
        idx (into-array Pointer
                        [(llvm/ConstInt (llvm-type (->IntegerType 64)) 0)])
        gbl (llvm/AddGlobal *module* (llvm-type (->ArrayType (->IntegerType 8)
                                                             (count ar)) )
                           (name (gensym "str_")))
        casted (llvm/ConstBitCast gbl
                                  (llvm-type (->PointerType (->IntegerType 8))))]
    (llvm/SetInitializer gbl llvm-ar)
    casted))



;; Common types
(def Int32 (->IntegerType 32))
(def Int32* (->PointerType (->IntegerType 32)))
(def Int64 (->IntegerType 64))
(def Int64* (->PointerType Int64))
(def Int1 (->IntegerType 1))
(def Int8 (->IntegerType 8))
(def I8* (->PointerType (->IntegerType 8)))
(def Int8* (->PointerType (->IntegerType 8)))
(def VoidT (->VoidType))

(def Float32 (->FloatType 32))
(def Float32* (->PointerType Float32))
(def Float32x4 (->VectorType Float32 4))
(def Float64 (->FloatType 64))
(def Float64* (->PointerType Float64))
(def Float64x4 (->VectorType Float64 4))
(def Float64x4* (->PointerType Float64x4))



