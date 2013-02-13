(ns mjolnir.logic-trees
  (:require [clojure.core.logic :refer :all]))

(def ^:dynamic *index*)
(def ^:dynamic *tree*)

(defmulti get-keys (fn [x]
                     (cond
                      (or (seq? x) (vector? x)) :vec
                      (associative? x) :map
                      :else :value)))

(defmethod get-keys :value [x]
  (assert false (str "Cant get keys on " x)))

(defmethod get-keys :vec
  [x]
  (range (count x)))

(defmethod get-keys :map
  [x]
  (keys x))

(defn -get-key-paths [x]
  (reduce (fn [accum key]
            (let [v (get x key)]
              (if (or (associative? v)
                      (vector? v)
                      (seq? v))
                (concat
                 accum
                 [[key]]
                 (map (partial cons key)
                      (-get-key-paths v)))
                accum)))
          []
          (get-keys x)))

(defn get-key-paths [x]
  (conj (-get-key-paths x)
        []))

(let [id (atom 0)]
  (defn gen-id []
    (-> (str "ID_" (swap! id inc))
        symbol
        (with-meta {:ref-id true}))))

(defn nil-update-in [mp path fn & args]
  (if (= path [])
    (apply fn mp args)
    (apply update-in mp path fn args)))

(defn assign-ids [x]
  (let [key-paths (get-key-paths x)
        id-pairs (map
                  vector
                  key-paths
                  (repeatedly gen-id))]
    {:tree (reduce (fn [tree [path id]]
                     (println tree path id)
                     (nil-update-in tree path #(with-meta %
                                             (assoc (meta %) :id id))))
                   x
                   id-pairs)
     :ids id-pairs}))


(defn get-value [v]
  (if-let [id (:id (meta v))]
    id
    v))

(defn gen-index [tree]
  (let [{:keys [tree ids]} (assign-ids tree)
        path-id (zipmap (map first ids)
                        (map second ids))
        id-path (zipmap (map second ids)
                        (map first ids))]
    {:path-id path-id
     :id-path id-path
     :eav (reduce (fn [acc path]
                    (let [id (get path-id path)
                          itm (get-in tree path)]
                      (reduce (fn [acc k]
                                (assoc-in acc [id k]
                                          (get-value (get itm k))))
                              acc
                              (get-keys itm))))
                  {}
                  (map first ids))
     :a-ev (reduce (fn [acc path]
                     (let [id (get path-id path)
                           itm (get-in tree path)]
                       (reduce (fn [acc k]
                                 (update-in acc [k]
                                            (fnil conj #{})
                                            [id (get-value (get itm k))]))
                               acc
                               (get-keys itm))))
                   {}
                   (map first ids))
     :ave (reduce (fn [acc path]
                     (let [id (get path-id path)
                           itm (get-in tree path)]
                       (reduce (fn [acc k]
                                 (update-in acc [k (get-value (get itm k))]
                                            (fnil conj #{})
                                            id))
                               acc
                               (get-keys itm))))
                   {}
                   (map first ids))
     :v-ea (reduce (fn [acc path]
                     (let [id (get path-id path)
                           itm (get-in tree path)]
                       (reduce (fn [acc k]
                                 (update-in acc [(get-value (get itm k))]
                                            (fnil conj #{})
                                            [id k]))
                               acc
                               (get-keys itm))))
                   {}
                   (map first ids))}))


(defn fresh? [x]
  (lvar? x))

(defn ground? [x]
  (not (lvar? x)))

(defn tree [id attr val]
  (fn [a]
    (let [wid (walk a id)
          wattr (walk a attr)
          wval (walk a val)]
      (println wid wattr wval a)
      (cond
       (and (fresh? wid) (ground? wattr) (fresh? wval))
       (-> (map
            (fn [[e v]]
              (unify a [wid wval] [e v]))
            ((:a-ev *index*) wattr))
           to-stream)

       (and (fresh? wid) (ground? wattr) (ground? wval))
       (-> (map
            (fn [e]
              (unify a wid e))
            (get-in (:ave *index*) [wattr wval]))
           to-stream)       

       (and (fresh? wid) (fresh? wattr) (ground? wval))
       (do (println ((:v-ea *index*) wval))
         (-> (map
              (fn [[e attr]]
                (unify a [wid wattr] [e attr]))
              ((:v-ea *index*) wval))
             to-stream))
       

       :else
       (println "failing" wid wattr wval a)))))

(defmacro query [ent vs & q]
  `(binding [*tree* ~ent]
     (binding [*index* (gen-index *tree*)]
       (println *index*)
       (time (vec (run* ~vs
                          ~@q))))))


