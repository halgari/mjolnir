(defproject mjolnir "0.1.5"
  :description "Compiler Toolkit using LLVM"
  :url "http://github.com/halgari/mjolnir"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.datomic/datomic-free "0.8.3826" ]
                 [org.clojure/core.logic "0.8.3"]
                 [net.java.dev.jna/jna "3.4.0"]
                 [criterium "0.3.1"]
		 #_[jcuda/jcuda "0.5.0"]]
  :test-paths ["test/"]
  :java-source-paths ["src/examples"]
  :jvm-opts ["-Djava.library.path=/usr/lib"
             "-Xmx4g"]
  :profiles {:ptx {:dependencies
                   []}})
