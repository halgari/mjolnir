(defproject mjolnir "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-RC2"]
                 [speclj "2.5.0"]
                 [net.java.dev.jna/jna "3.4.0"]
                 [fipp "0.1.0-SNAPSHOT"]]
  :plugins [[speclj "2.5.0"]]
  :test-paths ["spec/"]
  :dev-dependencies [[speclj-growl "1.0.1"]])
