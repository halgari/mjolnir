(ns mjolnir.config
  (:require [mjolnir.targets.target :as target]
            ))

(def ^:dynamic *target*)

;; These set the type that will be used to encode consts for both
;; integers and floats
(def ^:dynamic *float-type*)
(def ^:dynamic *int-type*)

(def ^:dynamic *builder*)
(def ^:dynamic *module*)
(def ^:dynamic *fn*)
(def ^:dynamic *llvm-fn*)
(def ^:dynamic *locals* {})
(def ^:dynamic *llvm-locals*)
(def ^:dynamic *llvm-recur-point*)
(def ^:dynamic *llvm-recur-phi*)
(def ^:dynamic *llvm-recur-block*)
(def ^:dynamic *block* (atom nil))
(def ^:dynamic *recur-point*)


(defmacro with-target [[nm target] & body]
  `(binding [*target* ~target]
     (let [~nm *target*]
       ~@body)))

(defmacro with-config [[int-type float-type target] & body]
  `(binding [*int-type* ~int-type
             *float-type* ~float-type
             *target* ~target]
     ~@body))

(def default-target-fn (atom nil))

(defn default-target []
  (require 'mjolnir.targets.darwin)
  (let [init (intern 'mjolnir.targets.darwin 'init-target)]
    (init (fn [d-fn]
              (reset! default-target-fn d-fn)))
    (@default-target-fn)))

