(ns mjolnir.ssa-rules
  (:require [clojure.pprint  :refer [pprint]]
            [datomic.api :refer [db q] :as d]))

(def rules (atom []))

(defmacro defrule [name args doc & body]
  #_(println "Registered rule" name )
  (swap! rules conj `[(~name ~@args)
                      ~@body])
  nil)

;; Utility

(defrule global-def [?id ?name ?type]
  "Functions are global entities"
  [?id :node/type :node.type/fn]
  [?id :fn/name ?name]
  [?id :fn/type ?type])

(defrule global-def [?id ?name ?type]
  "Functions are global entities"
  [?id :global/name ?name]
  [?id :global/type ?type])


;; Inference Rules

(defrule infer-node [?id ?attr ?val]
  "infer return-types"
  [?no-type :node/type :node.type/unknown]
  [?id :node/return-type ?no-type]
  (return-type ?id ?val)
  [(identity :node/return-type) ?attr])

#_(defrule infer-binop-node [?id ?attr ?val]
  "infer binop subtypes"
  (infer-binop ?id ?val)
  [(identity :inst.binop/sub-type) ?attr])

#_(defrule infer-node [?id ?attr ?val]
  "infer cmp"
  (infer-cmp-node ?id ?attr ?val))


(defrule infer-node [?id ?attr ?val]
  "infer casts"
  [?id :inst/type :inst.type/cast]
  [?id :inst.cast/type :inst.cast/unknown]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?arg0-t)
  [?id :node/return-type ?arg1-t]
  (cast-subtype ?id ?arg0-t ?arg1-t ?val)
  [(identity :inst.cast/type) ?attr])

;;


(defrule return-type [?id ?type]
  "Anything with :node/return-type returns that type"
  [?id :node/return-type ?type]
  [?no-type :node/type :node.type/unknown]
  [(not= ?type ?no-type)])

(defrule return-type [?id ?type]
  "Consts return their given type, if it exists"
  [?id :inst/type :inst.type/const]
  [?id :const/type ?type])

(defrule return-type [?id ?type]
  "Binops return the same type as their args"
  [?id :inst/type :inst.type/binop]
  [?id :inst.arg/arg0 ?arg0]
  [?id :inst.arg/arg1 ?arg1]
  #_(return-type ?arg0 ?type)
  (return-type ?arg1 ?type))

(defrule infer-phi-return-type [?id ?type]
  "Phi nodes always return the return type of one of their values"
  [?phi :phi.value/node ?id]
  [?phi :phi.value/value ?arg]
  [?arg :node/return-type ?type]
  #_(return-type ?arg ?type))

(defrule return-type [?id ?type]
  "Phi nodes always return the return type of one of their values"
  [?phi :phi.value/node ?id]
  [?phi :phi.value/value ?arg]
  #_[?arg :node/return-type ?type]
  (return-type ?arg ?type))

(defrule return-type [?id ?type]
  "Globals return the type of the matching global"
  [?id :inst/type :inst.type/gbl]
  [?id :inst.gbl/name ?name]
  (global-def ?gbl ?name ?type))

(defrule return-type [?id ?type]
  "Function calls return the return type of the function they are calling"
  [?id :inst/type :inst.type/call]
  [?id :inst.call/fn ?fn-src]
  (return-type ?fn-src ?fn-t)
  [?fn-t :type.fn/return ?type])

(defrule return-type [?id ?type]
  "Function pointer calls return the return type of the function they are calling"
  [?id :inst/type :inst.type/callp]
  [?id :inst.callp/fn ?fn-src]
  (return-type ?fn-src ?ptr-t)
  [?ptr-t :type/element-type ?fn-t]
  [?fn-t :type.fn/return ?type])

(defrule return-type [?id ?type]
  "Arg instructions return the type from the function type"
  [?id :inst/type :inst.type/arg]
  [?id :inst/block ?block]
  [?block :block/fn ?fn]
  [?fn :fn/type ?fn-t]
  [?arg-node :fn.arg/fn ?fn-t]
  [?id :inst.arg/idx ?idx]
  [?arg-node :fn.arg/idx ?idx]
  [?arg-node :fn.arg/type ?type])


(defrule return-type [?id ?type]
  "Store returns the ptr type"
  [?id :inst/type :inst.type/store]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?type))

(defrule return-type [?id ?type]
  "ASet returns the arr type"
  [?id :inst/type :inst.type/aset]
  [?id :inst.arg/arg0 ?arg0]
  [?arg0 :inst/type ?v]
  (return-type ?arg0 ?type))

(defrule return-type [?id ?type]
  "AGet returns the element type"
  [?id :inst/type :inst.type/aget]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?arg0-t)
  [?arg0-t :type/element-type ?type]
  [?type :node/type ?nt])

(defrule return-type [?id ?type]
  "Nth returns the same type as the input"
  [?id :inst/type :inst.type/nth]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?type))



(defrule member-idx [?tp ?nm ?idx ?member-tp]
  "Gets the idx of a member"
  [?mbr :type.member/struct ?tp]
  [?mbr :type.member/idx ?idx]
  [?mbr :type.member/name ?nm]
  [?mbr :type.member/type ?member-tp])

(defrule return-type [?id ?type]
  "Set returns the same type as the ptr"
  [?id :inst/type :inst.type/set]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?type))

(defrule return-type [?id ?type]
  "Get returns the member type"
  [?id :inst/type :inst.type/get]
  [?id :inst.arg/arg0 ?arg0]
  (return-type ?arg0 ?arg0-t)
  [?arg0-t :type/element-type ?etype]
  [?id :inst.get/member ?nm]
  (member-idx ?etype ?nm ?idx ?type))

(defrule return-type [?id ?type]
  "Atomic ops return the same type as the input value"
  [?id :inst/type :inst.type/atomic]
  [?id :inst.arg/arg1 ?arg1]
  (return-type ?arg1 ?type))





(defrule validate [?id ?msg]
  "Binops must have the same types for all args"
  [?id :inst/type :inst.type/binop]
  [?id :inst.arg/arg0 ?arg0]
  [?id :inst.arg/arg1 ?arg1]
  (return-type ?arg0 ?arg0-tp)
  (return-type ?arg1 ?arg1-tp)
  #_(return-type ?id ?this-tp)
  [(not= ?arg0-tp ?arg1-tp)]
  [(identity "Binop args must match return type") ?msg])


(defn func-arg-count-dont-match? [db tp-id call-id]
  (let [call-ent (d/entity db call-id)
        tp-ent (d/entity db tp-id)]
    (not= (count (:fn.arg/_fn tp-ent))
          ;; decrement this, as we include :inst.call/fn
          (dec (count (:inst/args call-ent))))))

(defrule validate [?id ?msg]
  "Calls must match argument counts"
  [?id :inst/type :inst.type/call]
  [?id :inst.call/fn ?gbl]
  [?gbl :inst.gbl/name ?name]
  [?gbl :node/return-type ?tp]
  [(mjolnir.ssa-rules/func-arg-count-dont-match? $ ?tp ?id)]
  [(str "Call signature doesn't match function, calling " ?name) ?msg])


(defrule validate [?id ?msg]
  "Args must match types"
  [?id :inst/type :inst.type/call]
  [?id :inst.arg/arg0 ?arg]
  [?arg :node/return-type ?arg-t]
  [?id :inst.call/fn ?fn]
  [?fn :inst.gbl/name ?name]
  [?fn :node/return-type ?fn-t]
  [?fn-arg :fn.arg/fn ?fn-t]
  [?fn-arg :fn.arg/idx 0]
  [?fn-arg :fn.arg/type ?fn-arg-t]
  [(not= ?fn-arg-t ?arg-t)]
  [?fn-arg-t :node/type ?fn-arg-t-node]
  [?arg-t :node/type ?arg-t-node]
  [?id :inst/block ?block]
  [?block :block/fn ?parent-fn]
  [?parent-fn :fn/name ?parent-fn-name]
  [(str "Arg0 doesn't match in call to "
        ?name
        " types "
        ?fn-arg-t-node
        " "
        ?arg-t-node
        " in "
        ?parent-fn-name) ?msg])

(defrule validate [?id ?msg]
  "Args must match types"
  [?id :inst/type :inst.type/call]
  [?id :inst.arg/arg1 ?arg]
  [?arg :node/return-type ?arg-t]
  [?id :inst.call/fn ?fn]
  [?fn :inst.gbl/name ?name]
  [?fn :node/return-type ?fn-t]
  [?fn-arg :fn.arg/fn ?fn-t]
  [?fn-arg :fn.arg/idx 1]
  [?fn-arg :fn.arg/type ?fn-arg-t]
  [(not= ?fn-arg-t ?arg-t)]
  [(str "Arg1 doesn't match in call to " ?name) ?msg])

(defrule validate [?id ?msg]
  "Args must match types"
  [?id :inst/type :inst.type/call]
  [?id :inst.arg/arg2 ?arg]
  [?arg :node/return-type ?arg-t]
  [?id :inst.call/fn ?fn]
  [?fn :inst.gbl/name ?name]
  [?fn :node/return-type ?fn-t]
  [?fn-arg :fn.arg/fn ?fn-t]
  [?fn-arg :fn.arg/idx 2]
  [?fn-arg :fn.arg/type ?fn-arg-t]
  [(not= ?fn-arg-t ?arg-t)]
  [(str "Arg2 doesn't match in call to " ?name) ?msg])




;; Binop rules - These rules define an attribute that helps emitters
;; decide if a binop is a Float or Int operation. FMul is different
;; from IMul, so this code specializes that information.

(comment
  (defrule infer-binop [?id ?op]
    "A binop of two ints "
    [?id :inst/type :inst.type/binop]
    [?id :inst.arg/arg0 ?arg0]
    [?id :inst.arg/arg1 ?arg1]
    (return-type ?arg0 ?arg0-t)
    (return-type ?arg1 ?arg1-t)
    (binop-subtype ?id ?arg0-t ?arg1-t ?op))


  (defrule binop-subtype [?type ?arg0-t ?arg1-t ?op]
    "Int + resolves to :iadd"
    [?arg0-t :node/type :type/int]
    [?arg1-t :node/type :type/int]
    [?type :inst.binop/type ?binop]
    [(mjolnir.ssa-rules/binop-int-translation ?binop) ?op])

  (defrule binop-subtype [?type ?arg0-t ?arg1-t ?op]
    "Float + resolves to :iadd"
    [?arg0-t :node/type :type/float]
    [?arg1-t :node/type :type/float]
    [?type :inst.binop/type ?binop]
    [(mjolnir.ssa-rules/binop-float-translation ?binop) ?op]))


;; Cast subtype

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Larger Ints truncate to smaller ints"
  [?arg0-t :node/type :type/int]
  [?arg1-t :node/type :type/int]
  [?arg0-t :type/width ?arg0-width]
  [?arg1-t :type/width ?arg1-width]
  [(> ?arg0-width ?arg1-width)]
  [(identity :inst.cast.type/trunc) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Smaller ints upcast to larger ints"
  [?arg0-t :node/type :type/int]
  [?arg1-t :node/type :type/int]
  [?arg0-t :type/width ?arg0-width]
  [?arg1-t :type/width ?arg1-width]
  [(< ?arg0-width ?arg1-width)]
  [(identity :inst.cast.type/zext) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Floats cast to int"
  [?arg0-t :node/type :type/float]
  [?arg1-t :node/type :type/int]
  [(identity :inst.cast.type/fp-to-si) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Ints cast to floats"
  [?arg0-t :node/type :type/int]
  [?arg1-t :node/type :type/float]
  [(identity :inst.cast.type/si-to-fp) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Pointers are bitcast"
  [?arg0-t :node/type :type/pointer]
  [?arg1-t :node/type :type/pointer]
  [(identity :inst.cast.type/bitcast) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Functions can be bitcast"
  [?arg0-t :node/type :type/fn]
  [?arg1-t :node/type :type/pointer]
  [(identity :inst.cast.type/bitcast) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Larger Ints truncate to smaller ints"
  [?arg0-t :node/type :type/pointer]
  [?arg1-t :node/type :type/int]
  [(identity :inst.cast.type/ptr-to-int) ?op])

(defrule cast-subtype [?id ?arg0-t ?arg1-t ?op]
  "Larger Ints truncate to smaller ints"
  [?arg0-t :node/type :type/int]
  [?arg1-t :node/type :type/pointer]
  [(identity :inst.cast.type/int-to-ptr) ?op])



;; Cmp predicate inference


(def cmp-map
  {[:type/int :type/int :inst.cmp.pred/=] :inst.cmp.sub-pred/int-eq
   [:type/int :type/int :inst.cmp.pred/not=] :inst.cmp.sub-pred/int-ne
   [:type/int :type/int :inst.cmp.pred/>] :inst.cmp.sub-pred/int-sgt
   [:type/int :type/int :inst.cmp.pred/<] :inst.cmp.sub-pred/int-slt
   [:type/int :type/int :inst.cmp.pred/<=] :inst.cmp.sub-pred/int-sle
   [:type/int :type/int :inst.cmp.pred/>=] :inst.cmp.sub-pred/int-sge

   [:type/float :type/float :inst.cmp.pred/=] :inst.cmp.sub-pred/real-oeq
   [:type/float :type/float :inst.cmp.pred/not=] :inst.cmp.sub-pred/real-one
   [:type/float :type/float :inst.cmp.pred/>] :inst.cmp.sub-pred/real-ogt
   [:type/float :type/float :inst.cmp.pred/<] :inst.cmp.sub-pred/real-olt
   [:type/float :type/float :inst.cmp.pred/<=] :inst.cmp.sub-pred/real-ole
   [:type/float :type/float :inst.cmp.pred/>=] :inst.cmp.sub-pred/real-oge})

#_(defrule infer-cmp-node [?id ?attr ?val]
  "Infer cmp predicate"
  [?id :inst/type :inst.type/cmp]
  [?id :inst.arg/arg0 ?arg0]
  [?id :inst.arg/arg1 ?arg1]
  [?id :inst.cmp/pred ?pred]
  (return-type ?arg0 ?arg0-t)
  (return-type ?arg1 ?arg1-t)
  [?arg0-t :node/type ?arg0-tg]
  [?arg1-t :node/type ?arg1-tg]
  [(vector ?arg0-tg ?arg1-tg ?pred) ?key]
  [(mjolnir.ssa-rules/cmp-map ?key) ?val]
  [(identity :inst.cmp/sub-pred) ?attr])





(comment

  ;; For a block, gets the instructions in the block
  (defrule instruction-seq [?block ?inst]
    "All instructions attached to the block should be considered"
    [?block :inst/block ?inst])

  (defrule instruction-seq [?block ?inst]
    "Terminator instructions should be considered"
    [?block :block/terminator-inst ?inst])

  (defrule depends-on [?block-a ?block-b]
    "Rule passes if block-a requires block-b before it can be built"
    [(mjolnir.ssa-rules/arg-k) [?attr ...]]
    (instruction-seq ?block-a ?inst-a)
    [?inst-a ?attr ?inst-b]
    [?inst-b :inst/block ?block-b]
    [(not= ?block-a ?block-b)])

  (defrule depends-on [?block-a ?block-b]
    "Rule passes if block-a requires block-b before it can be built"
    [(mjolnir.ssa-rules/arg-k) [?attr ...]]
    (instruction-seq ?block-a ?inst-a)
    [?inst-a ?attr ?inst-b]
    [?inst-b :phi/block ?block-b]
    [(not= ?block-a ?block-b)]))
