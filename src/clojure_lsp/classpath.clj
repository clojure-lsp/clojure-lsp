(ns clojure-lsp.classpath
  (:require
    [borkdude.deps :as deps]))

(defn lookup-deps-edn [db & extra-args]
  (apply deps/-main extra-args "-Spath"))


(comment
  (lookup-deps-edn nil "-A:test")
  )
