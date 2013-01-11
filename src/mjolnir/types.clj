(ns mjolnir.types
  (:require [mjolnir.llvmc :as llvm]))

(defmacro assure [pred]
  `(assert ~pred (str "at: " (pr-str (meta (:location ~'this)))
                      " got: " (pr-str ~(second pred)))))



(defprotocol Type
  (validate [this])
  (llvm-type [this]))

(defn valid? [tp]
  (validate tp)
  true)

(defprotocol ElementPointer
  (etype [this]))

(defn type? [this]
  (extends? Type (type this)))

(defn assure-type [tp]
  (assert (and (extends? Type (type tp))
               (valid? tp))
          (str "at: " (pr-str (meta (:location tp)))
                      " got: " (pr-str tp))))

(defrecord IntegerType [width]
  Type
  (validate [this]
    (assure (integer? width)))
  (llvm-type [this]
    (llvm/IntType width)))


(defrecord PointerType [etype]
  Type
  (validate [this]
    (assure-type etype))
  (llvm-type [this]
    (llvm/PointerType (llvm-type etype) 0))
  ElementPointer
  (etype [this]
    (:etype this)))

(defrecord FunctionType [arg-types ret-type]
  Type
  (validate [this]
    (every? assure-type arg-types)
    (assure-type ret-type))
  (llvm-type [this]
    (llvm/FunctionType (llvm-type ret-type)
                       (llvm/map-parr llvm-type arg-types)
                       (count arg-types)
                       false)))





;; Common types
(def Int32 (->IntegerType 32))
(def Int64 (->IntegerType 64))
(def I8* (->PointerType (->IntegerType 8)))


