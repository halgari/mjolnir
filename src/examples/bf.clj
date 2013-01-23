(ns examples.bf
  (:require [mjolnir.expressions :as exp]
            [mjolnir.constructors-init :as const]
            [mjolnir.types :refer [Int8 Int32 ->PointerType valid?]]
            [mjolnir.llvmc :as l]
            [mjolnir.config :as config]
            [mjolnir.targets.target :refer [emit-to-file]])
  (:alias c mjolnir.constructors))


(def cells (c/local "cells"))


(def Cells (->PointerType Int8))
(def RunCode-t (c/fn-t [] Int32))
(def Zero8 (exp/->ConstInteger 0 Int8))
(def One8 (exp/->ConstInteger 1 Int8))


(c/defn ^:extern ^:exact getchar [-> Int32])
(c/defn ^:extern ^:exact putchar [Int32 chr -> Int32])

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
  {:ip (c/iadd ip 1)
   :code (next code)})

(defmethod compile-bf \<
  [ip code]
  {:ip (c/isub ip 1)
   :code (next code)})

(defmethod compile-bf \+
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells
                      ip
                      (c/iadd (c/aget cells ip) One8))
              ip)
   :code (next code)})

(defmethod compile-bf \-
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells
                      ip
                      (c/isub (c/aget cells ip) One8))
              ip)
   :code (next code)})

(defmethod compile-bf \.
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (putchar (exp/->ZExt (c/aget cells ip) Int32))
              ip)
   :code (next code)})


(defmethod compile-bf \,
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells ip (exp/->Trunc (getchar) Int8))
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
        (compile-block (exp/->Local ip_name) (next code))
        ]
    
    {:ip (exp/->Loop [[ip_name ip]]
                     (c/if (c/is (c/aget cells (exp/->Local ip_name)) Zero8)
                           (exp/->Local ip_name)
                           (c/recur ret-ip -> Int32)))
     :code ret-code}))

(def hello-world "++++++++++[>+++++jhgj++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
#_(def hello-world "[-]")
#_(def hello-world "+++++++++++++++++++++++++++++++++>++[<.>-]")
#_(def hello-world ".+[.+]")

(defn mandelbrot []
  (println "Reading Code")
  (slurp "http://bfj.googlecode.com/svn-history/r3/trunk/bfj/bf/mandelbrot.b"))

(defn beer []
  (println "Reading Code")
  (slurp "http://www.73b.org/programs/beer.b"))

(defn -main [& opts]
  (println opts)
  (let [
        options (apply hash-map (map read-string opts))
        program-code (beer)
        _ (println "Building Expressions")
        cfn (const/c-fn "main" RunCode-t []
                        (c/using [cells (c/bitcast (c/malloc Int8 30000) Cells)]
                                 (c/dotimes [x 30000]
                                            (c/aset cells x Zero8))
                                 (loop [ip 0
                                        code program-code]
                                   (let [{ip :ip code :code} (compile-bf ip code)]
                                     #_(println "::::: " code)
                                     (if code
                                       (recur ip code)
                                       ip)))))]
    
    (let [target (config/default-target)]
      (let [_ (println "Compiling")
            built (exp/build (c/module ['examples.bf] cfn))
            _ (println "Writing Object File")
            compiled (time (emit-to-file target
                                         built
                                         options))]
        1
        #_(when-not output-file
          (println "Running")
          (time (-> compiled
                    (exp/run-exe)
                    (pr-str)
                    println)))))
    (println "Finished")
    (shutdown-agents)
    0))





