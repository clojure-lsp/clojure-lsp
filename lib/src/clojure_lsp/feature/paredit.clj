(ns clojure-lsp.feature.paredit
  (:require
   [clojure-lsp.logger :as logger]
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(defn ^:private paredit-op [paredit-fn move-cursor? uri original-zloc row col]
  (try
    (when original-zloc
      (let [pos-zloc (-> original-zloc
                         z/root-string
                         (z/of-string {:track-position? true}) ;; https://clojurians.slack.com/archives/CHB5Q2XUJ/p1740958982303849
                         (edit/find-at-pos row col))
            zloc (paredit-fn pos-zloc)
            {original-row :row original-col :col} (-> original-zloc z/node meta)
            offset-row (- row original-row)
            offset-col (- col original-col)
            [row' col'] (z/position zloc)
            new-row (+ row' offset-row)
            new-col (+ col' offset-col)
            root-zloc (z/up (edit/to-top zloc))]
        {:changes-by-uri {uri [{:loc root-zloc
                                :range (meta (z/node root-zloc))}]} ;; FIXME: range is always the whole document
         :show-document-after-edit {:uri uri
                                    :take-focus true
                                    :range (if move-cursor?
                                             (meta (z/node (z/up original-zloc)))
                                             {:row new-row
                                              :col new-col
                                              :end-row new-row
                                              :end-col new-col})}}))
    (catch Exception e
      (logger/error e))))

(def forward-slurp (partial paredit-op #(paredit/slurp-forward-into % {:from :current}) false))
(def forward-barf (partial paredit-op paredit/barf-forward false))
(def backward-slurp (partial paredit-op #(paredit/slurp-backward-into % {:from :current}) false))
(def backward-barf (partial paredit-op paredit/barf-backward false))
(def raise (partial paredit-op paredit/raise true))
(def kill (partial paredit-op paredit/kill false))
