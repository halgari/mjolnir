(ns mjolnir.transforms.ref-count
  (:require [clojure.core.logic :refer :all]
            [mjolnir.expressions :as expr]
            [mjolnir.types :as tp]
            [mjolnir.code-queries :as q]
            [mjolnir.logic-trees :refer :all])
  (:import [mjolnir.expressions Argument Fn IAdd Do]))

(defprotocol RefCountedExpr
  (ref-count [this input]))

(defn ref-counted? [a]
  (cond
   (tp/pointer-type? a) (= (q/gc-type a) :ref-count)
   (expr/Expression? a) (recur (expr/return-type a))))

(extend-protocol RefCountedExpr
  Fn
  (ref-count [this input]
    (assert (empty? input))
    (let [indexed-types (->> this
                             :type
                             :arg-types
                             (zipmap (range)))
          report (reduce
                  (fn [acc [idx tp]]
                    (if (ref-counted? tp)
                      (assoc-in acc [:args idx :last-read] (node-id this))
                      acc))
                  {}
                  indexed-types)
          report (ref-count (:body this) report)]
      (let [ret-id (-> report :leader :arg)]
        (reduce
         (fn [acc [idx tp]]
           (if (and (ref-counted? tp)
                    (not (= ret-id idx)))
             (update-in acc [:dec-refs :args idx] (fnil conj []) (node-id this))
             acc))
         report
         indexed-types))))
  Argument
  (ref-count [this input]
    (let [idx (:idx this)]
      (if (-> this :tp ref-counted?)
        (update-in input [:args idx :last-used] (node-id this))
        input)))
  Do
  (ref-count [this input]
    (reduce (fn [acc expr]
              (let [nr (ref-count expr acc)
                    ret-id (-> nr :leader :value)]
                (update-in acc [:dec-refs ret-id] (fnil conj []) (node-id this))
                acc))
            input
            (:body this))))

(defn ref-count-module [m]
  (->> m
       :body
       (filter (partial instance? Fn))
       (mapv (fn [f] [(:name f) (ref-count f {})]))))


(defn- ref-count-args [q]
  (fresh [id rt pid cid bid idx attr gc]
         (q/typeo id Argument)
         #_(tree id :idx idx)
         (q/return-typeo id rt)
         (predc rt (partial q/matches-gc-type? :ref-count))
         (q/parent-of-typeo pid Fn id)
         (== q {:key {:type :arg
                      :id idx
                      :fn-id pid}
                :return-type rt})))

(defn- ref-count-do [q]
  (fresh [id pid rt body-id idx node-id rt last-idx]
         (q/typeo id Do)
         (tree id :body body-id)
         (tree body-id idx node-id)
         (q/last-idxo body-id last-idx)
         (!= idx last-idx)
         (q/return-typeo node-id rt)
         (predc rt (partial q/matches-gc-type? :ref-count))
         (q/parent-of-typeo pid Fn id)
         (== q
            {:type :do
             :do-id id
             :return-type rt
             :idx idx})))

#_(defn- ref-count-calls [q]
  (fresh [id rt]
         ))

(defn query-refcnt-nodes [module]
  (query module
         [?q]
         (conde
          [(ref-count-args ?q)]
          [(ref-count-do ?q)])))



