(ns mjolnir.config
  (:require [mjolnir.targets.target :as target]
            [mjolnir.targets.darwin :as darwin]))

(def ^:dynamic *target*)

(defmacro with-target [[nm target] & body]
  `(binding [*target* ~target]
     (let [~nm *target*]
       ~@body)))

(def default-target-fn (atom nil))

(defn default-target []
  (darwin/init-target (fn [d-fn]
                        (reset! default-target-fn d-fn)))
  (@default-target-fn))