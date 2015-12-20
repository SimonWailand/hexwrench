(defproject hexwrench "0.2.0"
  :description "Tools for manipulating hex grids."
  :url "https://github.com/SimonWailand/hexwrench"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                #_(:cljs [org.clojure/clojurescript "1.7.145"]
                         [reagent "0.5.1"])]

  :source-paths ["src/clj"
                 "src/cljc"
                 "src/cljs"])
