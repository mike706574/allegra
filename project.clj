(defproject allegra "0.1.1"
  :license {:name "Eclipse Public License 1.0"
            :url "http://opensource.org/licenses/eclipse-1.0.php"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.csv "0.1.3"]
                 [clj-http "2.1.0"]
                 [com.taoensso/timbre "4.3.1"]]
  :resource-paths ["resources" "test-resources"]
  :main allegra.core
  :aot :all)
