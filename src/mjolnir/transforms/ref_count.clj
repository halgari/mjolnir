(ns mjolnir.transforms.ref-count
  (:require [clojure.core.logic :refer :all]
            [mjolnir.expressions :as expr]
            [mjolnir.types :as tp]
            [mjolnir.code-queries :as q]
            [mjolnir.logic-trees :refer :all]))


(defn query-refcnt-nodes [module]
  (query module
         [?id ?tp]
         (fresh [attr v]
                #_(tree ?id attr v)
                (q/return-typeo ?id ?tp)
                (predc ?tp (partial q/matches-gc-type? :ref-count)))))



