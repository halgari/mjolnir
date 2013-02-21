(defproject mjolnir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"}
  :dependencies [[org.clojure/clojure "1.5.0-RC2"]
                 [speclj "2.5.0"]
                 [net.java.dev.jna/jna "3.4.0"]
                 [fipp "0.1.0-SNAPSHOT"]
                 [criterium "0.3.1"]
                 [org.clojure/core.logic "0.8.0-rc2"]
                 [org.clojure/core.match "0.2.0-alpha11"]]
  :plugins [[speclj "2.5.0"]]
  :test-paths ["spec/"]
  :java-source-paths ["src/examples"]
  :profiles {:dev {:dependencies
                   [[speclj-growl "1.0.1"]
                    ]}})
