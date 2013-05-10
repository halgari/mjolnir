(defn fib [a]
  (if (= a 0)
    a
    (if (= a 1)
      a
      (+ (fib (+ a -1))
         (fib (+ a -2))))))

(defn -run []
  (fib 10))
