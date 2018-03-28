(defproject clojure-lsp "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.reader "1.2.1"]
                 [org.eclipse.lsp4j/org.eclipse.lsp4j "0.3.0" :exclusions [org.eclipse.xtend/org.eclipse.xtend.lib]]
                 [rewrite-clj "0.6.0"]
                 [org.eclipse.xtend/org.eclipse.xtend.lib "2.13.0" :exclusions [com.google.guava/guava]]
                 [com.google.guava/guava "19.0"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/core.async "0.4.474"]]
  :jvm-opts ^:replace ["-Xmx1g" "-server"])
