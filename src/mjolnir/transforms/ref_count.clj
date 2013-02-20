(ns mjolnir.transforms.ref-count
  (:require [clojure.core.logic :refer :all]
            [mjolnir.expressions :as expr]
            [mjolnir.types :as tp]
            [mjolnir.code-queries :as q]
            [mjolnir.logic-trees :refer :all])
  (:import [mjolnir.expressions Argument Fn IAdd Do Set Get Call Expression]))

(extend-protocol  IUninitialized
  Expression
  (-uninitialized [m]
    (reduce (fn [m x]
              (assoc m x nil))
            m
            (keys m))))

(defn ref-counted? [a]
  (cond
   (tp/pointer-type? a) (= (q/gc-type a) :ref-count)
   (expr/Expression? a) (recur (expr/return-type a))))

(comment

  (defprotocol RefCountedExpr
    (ref-count [this input]))



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
              (:body this)))))

(defprotocol ResourceFlowManager
  (provides [this] "Lists one or more resources provided by this node")
  (passes [this] "Lists one or more resources returned by this node")
  (terminates [this] "Lists one or more resources that are terminated at this node")
  (consumes [this] "Lists one or more resources that are read from by this node but not terminated by this same node"))


(defn defining-fn [id]
  (loop [path (-> *index* :id-path (get id))]
    (let [nd (get-in *tree* path)]
      (if (or (instance? Fn nd)
              (empty? path))
        (:id (meta nd))
        (recur (pop path))))))

(extend-protocol ResourceFlowManager
  Argument
  (provides [this]
    [])
  (passes [this]
    (when (ref-counted? this)
      [[{:fn (defining-fn (:id (meta this)))
         :idx (:idx this)}
        (node-id this)]]))
  (terminates [this]
    [])
  (consumes [this]
    [])
  
  Do
  (provides [this]
    [])
  (passes [this]
    (when (ref-counted? this)
      [[(node-id (last (:body this)))
        (node-id this)]]))
  (terminates [this]
    [])
  (consumes [this]
    [])

  Call
  (provides [this]
    (when (ref-counted? this)
      [(node-id this)]))
  (passes [this]
    [])
  (terminates [this]
    (reduce (fn [acc x]
              (if (ref-counted? x)
                (conj acc (node-id x))
                acc))
            []
            (:args this)))
  (consumes [this]
    [])

  Fn
  (provides [this]
    (reduce (fn [acc [tp idx]]
              (if (ref-counted? tp)
                (conj acc {:fn (:id (meta this))
                           :idx idx})
                acc))
            []
            (zipmap (-> this :type :arg-types)
                    (range))))
  (passes [this]
    (when (ref-counted? (:body this))
      [[(node-id (:body this))
        (node-id this)]]))
  (terminates [this]
    [])
  (consumes [this]
    [])

  Set
  (provides [this]
    [])
  (passes [this]
    (when (ref-counted? this)
      [[(node-id (:ptr this))
        (node-id this)]]))
  (terminates [this]
    (when (ref-counted? (:val this))
      [(node-id (:val this))]))
  (consumes [this]
    [])

  Get
  (provides [this]
    (when (ref-counted? this)
      [(node-id this)]))
  (passes [this]
    [])
  (terminates [this]
    [])
  (consumes [this]
    (when (ref-counted? (:ptr this))
      [(node-id (:ptr this))])))

(defn- make-relation [f]
  (q/filter-map-relation #(and
                           (expr/Expression? %)
                           (extends? ResourceFlowManager (type %)))
                         f))

(def provideso (make-relation provides))
(def passeso (make-relation passes))
(def terminateso (make-relation terminates))
(def consumeso (make-relation consumes))


(defn find-terminators [resource loc end-type]
  (conde
   [(== resource nil)
    fail]
   [(!= resource nil)
    (conde
     [(fresh [terminators]
             (terminateso loc terminators)
             (membero resource terminators)
             (== end-type :terminated))]
     [(fresh [consumes]
             (consumeso loc consumes)
             (membero resource consumes)
             (== end-type :consumed))]
     [(fresh [renames to _]
             (passeso _ renames)
             (membero [resource to] renames)
             (find-terminators to loc end-type))])]))


(defn do-gc [m]
  (query m
         [a q b tp]
         (conde
          [(provideso a b )
           (== q :provides)
           (q/typeo a tp)]
          [(passeso a b )
           (== q :passes)
           (q/typeo a tp)]
          [(terminateso a b )
           (== q :terminates)
           (q/typeo a tp)]
          [(consumeso a b )
           (== q :consumes)
           (q/typeo a tp)]))
  (query m
         [?q]
         (fresh [resources resource start end end-type]
                (provideso start resources)
                (membero resource resources)
                (find-terminators resource end end-type)
                (== ?q {:start start
                        :resource resource
                        :end end
                        :end-type end-type}))))




#_(defn ref-count-module [m]
  (->> m
       :body
       (filter (partial instance? Fn))
       (mapv (fn [f] [(:name f) (ref-count f {})]))))

(comment

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



)