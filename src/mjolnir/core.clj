(ns mjolnir.core
  (:require
   [mjolnir.types :refer [Int64 Float64]]
   [mjolnir.targets.target :refer [as-dll]]
   [mjolnir.expressions :as expr]
   [mjolnir.inference :refer [infer-all]]
   [mjolnir.validation :refer [validate]]
   [clojure.test :refer :all]
   [datomic.api :refer [q db] :as d]
   [mjolnir.config :refer [*int-type* *float-type* *target* default-target]]
   [mjolnir.ssa :refer :all]
   [mjolnir.llvm-builder :refer [build dump optimize verify]]))

(defn to-db [m]
  (let [conn (new-db)]
    (-> (gen-plan
         [_ (add-to-plan m)]
         nil)
        (get-plan conn)
        commit)
    {:conn conn}))

(defn to-llvm-module [{:keys [conn] :as ctx}]
  (infer-all conn)
  (validate (db conn))
  (let [built (build (db conn))]
    (verify built)
    (optimize built)
    #_(dump built)
    (assoc ctx :module built)))

(defn to-dll [{:keys [module] :as ctx}]
  (assoc ctx :dll (as-dll (default-target) module {:verbose true})))

(defn get-fn [{:keys [conn module dll]} ctr]
  (let [nm (-> (ctr) :fnc :name)
        _ (assert nm (str "Cant get name " nm))
        db-val (db conn)
        ent (ffirst (q '[:find ?id
                        :in $ ?nm
                        :where
                        [?id :fn/name ?nm]]
                      db-val
                      nm))
        _ (assert ent (str "Can't find " nm))
        ent (d/entity db-val ent)]
    (assert ent (pr-str "Can't find " nm))
    (get dll ent)))

(defn build-module [m]
  (-> (to-db m)
      (to-llvm-module)))

(defn build-default-module [m]
  (binding [*int-type* Int64
            *float-type* Float64
            *target* (default-target)]
    (-> (to-db m)
        (to-llvm-module)
        (to-dll))))




