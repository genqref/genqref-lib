(defproject genqref-lib "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [com.github.oliyh/martian "0.1.23"]
                 [com.github.oliyh/martian-clj-http "0.1.23"]
                 [com.taoensso/timbre "6.1.0"]
                 [duratom "0.5.8"]
                 [cheshire "5.11.0"]]
  :repl-options {:init-ns genqref-lib.core})
