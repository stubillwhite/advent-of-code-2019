(defproject advent-of-code-2019 "0.1.0-SNAPSHOT"

  :description "TODO"

  :url "TODO"

  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :repl-options {:port 4555}

  :plugins []
  
  :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                 [org.clojure/clojure "1.10.1"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/tools.trace "0.7.10"]
                 [mount "0.1.16"]]
  
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.3.1"]]
                   :source-paths ["dev"]}})
