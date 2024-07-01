(ns clojure-lsp.feature.paredit
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(defn ^:private paredit-op [paredit-fn move-cursor? uri original-zloc]
  (try
    (when original-zloc
      (let [zloc (paredit-fn original-zloc)
            root-zloc (z/up (edit/to-top zloc))]
        (shared/assoc-some
          {:changes-by-uri {uri [{:loc root-zloc
                                  :range (meta (z/node root-zloc))}]}}
          :show-document-after-edit (when move-cursor?
                                      {:uri uri
                                       :take-focus true
                                       :range (meta (z/node (z/up original-zloc)))}))))
    (catch Exception e
      (logger/error e))))

(def forward-slurp (partial paredit-op paredit/slurp-forward false))
(def forward-barf (partial paredit-op paredit/barf-forward false))
(def backward-slurp (partial paredit-op paredit/slurp-backward false))
(def backward-barf (partial paredit-op paredit/barf-backward false))
(def raise (partial paredit-op paredit/raise true))
(def kill (partial paredit-op paredit/kill false))
