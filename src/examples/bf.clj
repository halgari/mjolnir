(ns examples.bf
  (:require [mjolnir.expressions :as exp]
            [mjolnir.inference :refer [infer-all]]
            [mjolnir.validation :refer [validate]]
            [mjolnir.constructors-init :as const]
            [mjolnir.types :refer [Int64 ->PointerType valid? ->ArrayType]]
            [mjolnir.llvmc :as l]
            [mjolnir.config :as config]
            [mjolnir.targets.target :refer [emit-to-file]]
            [mjolnir.core :as core])
  (:alias c mjolnir.constructors))


(def cells (c/local "cells"))


(def Cells (->PointerType Int64))
(def RunCode-t (c/fn-t [] Int64))


(c/defn ^:extern ^:exact getchar [-> Int64])
(c/defn ^:extern ^:exact putchar [Int64 chr -> Int64])

(defmulti compile-bf (fn [ip code] (first code)))


(defn compile-block [ip code]
  (loop [ip ip
         code code]
    (if (= \] (first code))
      {:code (next code)
       :ip ip}
      (let [c (compile-bf ip code)]
        (recur (:ip c)
               (:code c))))))

(defmethod compile-bf :default
  [ip code]
  {:ip ip
   :code (next code)})

(defmethod compile-bf \>
  [ip code]
  {:ip (c/+ ip 1)
   :code (next code)})

(defmethod compile-bf \<
  [ip code]
  {:ip (c/- ip 1)
   :code (next code)})

(defmethod compile-bf \+
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells
                      ip
                      (c/+ (c/aget cells ip) 1))
              ip)
   :code (next code)})

(defmethod compile-bf \-
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells
                      ip
                      (c/- (c/aget cells ip) 1))
              ip)
   :code (next code)})

(defmethod compile-bf \.
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (putchar (c/aget cells ip))
              ip)
   :code (next code)})


(defmethod compile-bf \,
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells ip (getchar))
        ip)
   :code (next code)})


(comment
  ;; loop logic

  (loop [ip in-ip]
    (if (aget = 0)
      ip))

  )


(defmethod compile-bf \[
  [ip code]
  (let [ip_name (name (gensym "ip_"))
        {ret-code :code ret-ip :ip}
        (compile-block (exp/->Local ip_name) (next code))]
    
    {:ip (exp/->Loop [[ip_name ip]]
                     (c/if (c/= (c/aget cells (exp/->Local ip_name)) 0)
                           (exp/->Local ip_name)
                           (c/recur ret-ip)))
     :code ret-code}))

(defn -main [program & opts]
  (println opts)
  (let [options (apply hash-map (map read-string opts))
        program-code (slurp program)
        _ (println "Building Expressions")
        
        cfn (const/c-fn "main" RunCode-t []
                        nil
                        (c/using [cells (c/bitcast (c/malloc (->ArrayType Int64 30000)) Cells)]
                                 (c/dotimes [x 30000]
                                            (c/aset cells x 0))
                                 (loop [ip 0
                                        code program-code]
                                   (let [{ip :ip code :code} (compile-bf ip code)]

                                     (if code
                                       (recur ip code)
                                       ip)))))]

    (try
      (binding [config/*target* (config/default-target)
                config/*int-type* Int64]
        (let [_ (println "Compiling")
              module (c/module ['examples.bf]
                               cfn)
              conn (core/to-db module)
              built (core/to-llvm-module conn)
              
              optimized built
              _ (println "Writing Object File")
              compiled (time (emit-to-file config/*target*
                                           optimized
                                           options))]))
      (println "Finished")
      (finally
       (shutdown-agents)))
    0))





