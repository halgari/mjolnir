(ns mjolnir.core
  (:require
   [mjolnir.expressions :as expr]
   [mjolnir.inference :refer [infer-all]]
   [mjolnir.validation :refer [validate]]
   [clojure.test :refer :all]
   [datomic.api :refer [q db] :as d]
   [mjolnir.config :refer [*int-type* *target* default-target]]
   [mjolnir.ssa :refer :all]
   [mjolnir.llvm-builder :refer [build dump optimize verify]]))

(defn to-db [m]
  (let [conn (new-db)]
    (-> (gen-plan
         [_ (add-to-plan m)]
         nil)
        (get-plan conn)
        commit)
    conn))

(defn to-llvm-module [conn]
  (infer-all conn)
  (validate (db conn))
  (let [built (build (db conn))]
    #_(dump built)
    (verify built)
    (optimize built)
    built))

(defn build-module [m]
  (-> (to-db m)
      (to-llvm-module)))



