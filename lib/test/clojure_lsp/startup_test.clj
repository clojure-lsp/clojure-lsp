(ns clojure-lsp.startup-test
  (:require
   [clojure-lsp.startup :as startup]
   [clojure.test :refer [are deftest]]))

(deftest consider-local-db-cache?-test
  (are [session-analysis-type cache-analysis-type result]
       (= result
          (#'startup/consider-local-db-cache?
           {:project-analysis-type session-analysis-type}
           {:project-analysis-type cache-analysis-type}))
    :project-and-full-dependencies :project-and-full-dependencies true
    :project-and-full-dependencies :project-and-shallow-analysis false
    :project-and-full-dependencies :project-only false

    :project-and-shallow-analysis :project-and-full-dependencies true
    :project-and-shallow-analysis :project-and-shallow-analysis true
    :project-and-shallow-analysis :project-only false

    :project-only :project-and-full-dependencies true
    :project-only :project-and-shallow-analysis true
    :project-only :project-only true))
