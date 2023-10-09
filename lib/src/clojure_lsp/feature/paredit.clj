(ns clojure-lsp.feature.paredit
  (:require
   [clojure-lsp.logger :as logger]
   [rewrite-clj.paredit :as paredit]
   [rewrite-clj.zip :as z]))

(defn ^:private paredit-op [paredit-fn zloc]
  (try
    (when zloc
      (let [zloc (paredit-fn zloc)
            zloc (or (some-> zloc z/up)
                     zloc)
            zloc (or (some-> zloc z/up)
                     zloc)]
        [{:loc zloc
          :range (meta (z/node zloc))}]))
    (catch Exception e
      (logger/error e))))

(def forward-slurp (partial paredit-op paredit/slurp-forward))
(def forward-barf (partial paredit-op paredit/barf-forward))
(def backward-slurp (partial paredit-op paredit/slurp-backward))
(def backward-barf (partial paredit-op paredit/barf-backward))
(def raise (partial paredit-op paredit/raise))
(def kill (partial paredit-op paredit/kill))
