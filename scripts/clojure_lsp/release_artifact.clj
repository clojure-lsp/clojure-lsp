(ns clojure-lsp.release-artifact
  (:require
   [babashka.process :refer [sh]]
   [borkdude.gh-release-artifact :as ghr]
   [clojure.string :as str]))

(defn current-branch []
  (or (System/getenv "APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH")
      (System/getenv "APPVEYOR_REPO_BRANCH")
      (System/getenv "CIRCLE_BRANCH")
      (System/getenv "GITHUB_REF_NAME")
      (System/getenv "CIRRUS_BRANCH")
      (-> (sh "git rev-parse --abbrev-ref HEAD")
          :out
          str/trim)))

(defn tag []
  (->> (sh "git tag --points-at HEAD")
       :out
       str/trim
       not-empty))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn release [& args]
  (let [latest-dev-tag (:tag_name (first (ghr/list-releases "clojure-lsp" "clojure-lsp-dev-builds")))
        prod-release-tag (tag)
        gh-token (System/getenv "GITHUB_TOKEN")
        file (first args)
        branch (current-branch)
        release-branch? (contains? #{"master" "main"} branch)]
    (assert file "File name must be provided")
    (cond
      (not gh-token)
      (println "Skipping: no GITHUB_TOKEN found")

      (not release-branch?)
      (println (format "Skipping: not on release branch (%s)" branch))

      prod-release-tag
      (ghr/overwrite-asset {:org "clojure-lsp"
                            :repo "clojure-lsp"
                            :file file
                            :tag (str/trim (slurp "lib/resources/CLOJURE_LSP_VERSION"))})

      latest-dev-tag
      (ghr/overwrite-asset {:org "clojure-lsp"
                            :repo "clojure-lsp-dev-builds"
                            :file file
                            :tag latest-dev-tag}))
    nil))
