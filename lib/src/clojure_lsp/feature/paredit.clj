(ns clojure-lsp.feature.paredit
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(defn ^:private paredit-op [paredit-fn move-cursor? uri original-zloc row col]
  (try
    (when original-zloc
      (let [zloc (paredit-fn original-zloc)
            root-zloc (z/up (edit/to-top zloc))]
        {:changes-by-uri {uri [{:loc root-zloc
                                :range (meta (z/root original-zloc))}]}
         :show-document-after-edit {:uri uri
                                    :take-focus true
                                    :range (if move-cursor?
                                             (meta (z/node (z/up original-zloc)))
                                             {:row row :col col :end-row row :end-col col})}}))
    (catch Exception e
      (logger/error e))))

(def forward-slurp (partial paredit-op #(paredit/slurp-forward-into % {:from :current}) false))
(def forward-barf (partial paredit-op paredit/barf-forward false))
(def backward-slurp (partial paredit-op paredit/slurp-backward false))
(def backward-barf (partial paredit-op paredit/barf-backward false))
(def raise (partial paredit-op paredit/raise true))
(def kill (partial paredit-op paredit/kill false))
