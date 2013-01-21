(ns examples.bf
  (:require [mjolnir.expressions :as exp]
            [mjolnir.constructors-init :as const]
            [mjolnir.types :refer [Int32 ->PointerType valid?]])
  (:alias c mjolnir.constructors))


(def cells (c/local "cells"))

(c/defn ^:extern ^:exact getchar [-> Int32] 0)
(c/defn ^:extern ^:exact putchar [Int32 chr -> Int32] 0)

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
                      (c/iadd (c/aget cells ip) 1))
              ip)
   :code (next code)})

(defmethod compile-bf \-
  [in-ip code]
  {:ip (c/let [ip in-ip]
              (c/aset cells
                      ip
                      (c/isub (c/aget cells ip) 1))
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
  (let [{ret-code :code ret-ip :ip}
        (compile-block (c/local "ip") (next code))]
    {:ip (exp/->Loop [["ip" ip]]
                     (c/if (c/is (c/aget cells (c/local "ip")) 0)
                           (c/local "ip")
                           (c/recur ret-ip -> Int32)))
     :code ret-code}))

#_(def hello-world "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.")
(def hello-world "+++>>>>++<<<--")

(def Cells (->PointerType Int32))
(def RunCode-t (c/fn-t [Cells] Int32)) 

(defn -main []
  (let [cfn (const/c-fn "run-code" RunCode-t [cells]
                        (c/let [cells (c/malloc Int32 20000)]
                          (exp/pdebug (loop [ip 0
                                             code hello-world]
                                        (let [{ip :ip code :code} (compile-bf ip code)]
                                          #_(println "::::: " code)
                                          (if (next code)
                                            (recur ip code)
                                            ip))))))]
    
    
    (valid? (exp/pdebug (c/module ['examples.bf]
                                  cfn)))
    (Thread/sleep 1000)
    (exp/build (c/module ['examples.bf]
                         cfn))))





