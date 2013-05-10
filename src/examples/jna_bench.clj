(ns examples.jna-bench
  (:require [criterium.core :as crit])
  (:import [com.sun.jna Native Pointer Memory Function Platform]))

(set! *warn-on-reflection* true)

(defn cos-invoke ^Function []
  (com.sun.jna.Function/getFunction Platform/C_LIBRARY_NAME "cos"))


(defn -main []
  (let [f (cos-invoke)]
    (crit/quick-bench
     (.invoke f Double (object-array [0.5])))))

