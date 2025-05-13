(ns ci
  (:require
   [clojure.string :as string]))

(defn ^:private make-literal [a]
  (.replace a "\"" "\\\""))

(defn ^:private extract-text-between [prefix suffix from-string]
  (let [pattern (str (make-literal prefix) "([\\s\\S]*?)" (make-literal suffix))]
    (second (re-find (re-pattern pattern) from-string))))

#_{:clojure-lsp/ignore [:clojure-lsp/unused-public-var]}
(defn get-last-changelog-entry [version]
  (println (->> (slurp "CHANGELOG.md")
                (extract-text-between (str "## " version) "## ")
                string/trim)))
