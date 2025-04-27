(ns clojure-lsp.diff-test
  (:require
   [clojure-lsp.diff :as diff]
   [clojure.string :as string]
   [clojure.test :as t :refer [deftest is testing]]
   [matcher-combinators.test :refer [match?]]))

(deftest ->chunks-test
  (let [mkdocs-diff (string/join
                      "\n"
                      ["diff --git a/mkdocs.yml b/mkdocs.yml"
                       "index 295627a2..b8d63bf2 100644"
                       "--- a/mkdocs.yml"
                       "+++ b/mkdocs.yml"
                       "@@ -1,5 +1,4 @@"
                       " site_name: Clojure LSP"
                       "-site_description: Language Server Protocol Implementation for Clojure"
                       " site_url: https://clojure-lsp.io"
                       ""
                       " repo_name: clojure-lsp/clojure-lsp"])
        mkdocs-expected {:file "mkdocs.yml"
                         :added-line-numbers []}
        internal-api-diff (string/join
                            "\n"
                            ["diff --git a/lib/src/clojure_lsp/internal_api.clj b/lib/src/clojure_lsp/internal_api.clj"
                             "index 58e07f28..bd4b99af 100644"
                             "--- a/lib/src/clojure_lsp/internal_api.clj"
                             "+++ b/lib/src/clojure_lsp/internal_api.clj"
                             "@@ -356,10 +356,11 @@"
                             "   [diff-out diags-by-uri]"
                             "   (let [diags-by-uri-keys (keys diags-by-uri)"
                             "         chunks (diff/->chunks diff-out)"
                             "+        chunks-with-additions (filter #(-> % :added-lines seq) chunks)"
                             "         chunks-by-file (reduce (fn [acc {:keys [file] :as hunk}]"
                             "                                  (update acc file (fnil conj []) hunk))"
                             "                                {}"
                             "-                               chunks)"
                             "+                               chunks-with-additions)"
                             "         chunks-by-uri (reduce (fn [acc [file hunks]]"
                             "                                 (if-let [uri (some #(when (string/ends-with? % file) %)"
                             "                                                    diags-by-uri-keys)]"])
        internal-api-expected {:file "lib/src/clojure_lsp/internal_api.clj"
                               :added-line-numbers [359 363]}]
    (testing "Given a diff output with only a deletion
              When parse the text
              Then it has empty :added-line-numbers"
      (is (match? [mkdocs-expected]
                  (diff/->chunks mkdocs-diff))))
    (testing "Given a diff output with two additions and one deletion
              When parse the text
              Then it has :added-line-numbers"
      (is (match? [internal-api-expected]
                  (diff/->chunks internal-api-diff))))
    (testing "Given a diff output with two files
              When parse the text
              Then the first chunk has no :added-line-numbers
              And the second one has :added-line-numbers"
      (is (match? [mkdocs-expected internal-api-expected]
                  (diff/->chunks (str mkdocs-diff
                                      "\n"
                                      internal-api-diff)))))))
