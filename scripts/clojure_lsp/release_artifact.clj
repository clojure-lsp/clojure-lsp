(ns clojure-lsp.release-artifact
  (:require
   [babashka.process :refer [sh]]
   [borkdude.gh-release-artifact :as ghr]
   [clojure.string :as str]))

(defn current-branch []
  (or (System/getenv "APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH")
      (System/getenv "APPVEYOR_REPO_BRANCH")
      (System/getenv "CIRCLE_BRANCH")
      (-> (sh ["git" "rev-parse" "--abbrev-ref" "HEAD"])
          :out
          str/trim)))

(defn tag []
  (->> (sh "git tag --points-at HEAD")
       :out
       str/trim
       not-empty))

(defn release [& args]
  (let [t (tag)]
    (println "The tag: " t)
    (if t
      (let [current-version (-> (slurp "lib/resources/CLOJURE_LSP_VERSION")
                                str/trim)
            ght (System/getenv "GITHUB_TOKEN")
            file (first args)
            branch (current-branch)
            release-branch? (contains? #{"master" "main"} branch)]
        (when-not ght
          (println "Skipping: not GITHUB_TOKEN"))
        (when-not release-branch?
          (println "Skipping: not on release branch"))
        (if (and ght release-branch?)
          (do (assert file "File name must be provided")
              (ghr/overwrite-asset {:org "clojure-lsp"
                                    :repo "clojure-lsp"
                                    :file file
                                    :tag (str "v" current-version)}))
          (println "Skipping release artifact (no GITHUB_TOKEN or not on main branch)"))
        nil)
      (println "Skipping release artifact because no tag"))))
