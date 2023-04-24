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
    :project-and-full-dependencies :project-and-clojure-only-dependencies false
    :project-and-full-dependencies :project-only false

    :project-and-clojure-only-dependencies :project-and-full-dependencies true
    :project-and-clojure-only-dependencies :project-and-clojure-only-dependencies true
    :project-and-clojure-only-dependencies :project-only false

    :project-only :project-and-full-dependencies true
    :project-only :project-and-clojure-only-dependencies true
    :project-only :project-only true))
