(defproject mjolnir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [com.datomic/datomic-free "0.8.3826" ]
                 [speclj "2.5.0"]
                 [net.java.dev.jna/jna "3.4.0"]
                 [criterium "0.3.1"]
                 [midje "1.5-RC1"]]
  :plugins [[speclj "2.5.0"]]
  :test-paths ["test/"]
  :java-source-paths ["src/examples"]
  :jvm-opts ["-Djava.library.path=/usr/lib"]
  :profiles {:ptx {:dependencies
                   [[jcuda/jcuda "0.5.0"]]}})
