(defproject clj4ds "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [incanter "1.5.5"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [me.raynes/fs "1.4.6"]]
  :resource-paths ["data"]
  :profiles {:dev {:dependencies [[org.clojure/tools.cli "0.3.1"]]}}
  :jvm-opts ["-Xmx2G"])
