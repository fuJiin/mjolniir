(defproject fujiin.mjolniir "0.0.1-SNAPSHOT"
  :description "Clojurizing Mallet"
  :url "http://github.com/fuJiin/mjolniir"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [marcliberatore.mallet-lda "0.1.1"]
                 [cheshire "5.2.0"]]
  :jvm-opts ["-Xmx512m"]
  :profiles {:dev {:dependencies [[clj-http "0.7.7"]
                                  [hickory "0.5.2"]
                                  [enlive "1.1.4"]]}})
