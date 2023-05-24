(ns clojure-lsp.feature.replace-refer-all
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn replace-with-refers [loc refers]
  (let [refers-loc (z/edit-> loc
                             (edit/z-replace-preserving-meta (n/vector-node (->> refers
                                                                                 (mapv #(n/token-node (symbol %)))
                                                                                 (interpose (n/spaces 1))))))]
    [{:range (meta (z/node refers-loc))
      :loc refers-loc}]))

(defn replace-with-alias [loc uri db]
  (let [ns (z/sexpr (z/find-tag (z/leftmost loc) z/next :token))
        usages (q/find-local-var-usages-to-namespace db uri ns)
        ;; The LSP spec does not support asking question with custom answer from client.
        ;; Should we create a custom request/response for that? could be useful in other places.
        fake-alias "an-alias"
        alias-loc (z/up
                    (z/edit-> loc
                              (z/replace (with-meta (n/token-node (symbol fake-alias))
                                                    (meta (z/node loc))))
                              (z/find-token z/prev #(= ":refer" (z/string %)))
                              (edit/z-replace-preserving-meta (n/keyword-node :as))))]
    (concat [{:range (meta (z/node alias-loc))
              :loc alias-loc}]
            (when (seq usages)
              (mapv (fn [usage]
                      (let [zloc (z/find-next-tag (edit/find-at-usage loc usage) z/next :token)
                            zloc (z/edit-> zloc
                                           (edit/z-replace-preserving-meta (n/token-node (symbol fake-alias (str (:name usage))))))]
                        {:range (meta (z/node zloc))
                         :loc zloc}))
                    usages)))))
