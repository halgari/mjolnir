(ns mjolnir.types
  (:require [mjolnir.llvmc :as llvm]))

(defmacro assure [pred]
  `(assert ~pred (str "at: " (pr-str (meta (:location ~'this)))
                      " got: " (pr-str ~(second pred)))))


(defmacro validate-all [& body]
  `(apply valid? ~(vec body)))

(defmacro assure-same-type [& body]
  `(reduce (fn [a# x#]
             (assert (= a# x#)
                     (str "Expected same types, "
                      "at: " (pr-str (meta (:location ~'this)))
                      " got: " (pr-str ~(vec body)))))
           ~(vec body)))

(defprotocol Validatable
  (validate [this]))

(defprotocol Type
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
  Validatable
  (validate [this]
    (assure (integer? width)))
  Type
  (llvm-type [this]
    (llvm/IntType width)))


(defrecord PointerType [etype]
  Validatable
  (validate [this]
    (assure-type etype))
  Type
  (llvm-type [this]
    (llvm/PointerType (llvm-type etype) 0))
  ElementPointer
  (etype [this]
    (:etype this)))

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
                       false)))

(defn FunctionType? [tp]
  (instance? FunctionType tp))




;; Common types
(def Int32 (->IntegerType 32))
(def Int64 (->IntegerType 64))
(def Int1 (->IntegerType 1))
(def I8* (->PointerType (->IntegerType 8)))


