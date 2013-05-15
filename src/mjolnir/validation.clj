(ns mjolnir.validation
  (:require [mjolnir.ssa :as ssa]
            [mjolnir.ssa-rules :refer [rules]]
            [datomic.api :refer [q db] :as d]))


(defn get-errors [db]
  (q '[:find ?id ?msg
       :in $ %
       :where
       (validate ?id ?msg)]
     db
     @rules))

(defn validate [db-val]
  (let [errors (get-errors db-val)]
    (assert (zero? (count (get-errors db-val)))
            (str "Errors: " (vec (for [[id msg] errors]
                                   [msg (d/touch (d/entity db-val id))]))))))