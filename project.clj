(defproject com.github.genqref/genqref-lib "0.1.0-SNAPSHOT"
  :description "batteries-included API Wrapper for the great Space Traders API "
  :url "https://github.com/genqref/genqref-lib"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.github.oliyh/martian "0.1.23"]
                 [com.github.oliyh/martian-httpkit "0.1.23"]
                 [com.taoensso/timbre "6.1.0"]
                 [duratom "0.5.8"]
                 [cheshire "5.11.0"]
                 [slingshot "0.12.2"]]
  :repl-options {:init-ns genqref-lib.core}
  :repositories [["releases" {:url "https://repo.clojars.org"}]]
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]])
