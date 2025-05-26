(ns clojure-lsp.feature.cycle-keyword
  (:require
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn cycle-keyword-auto-resolve-status [zloc]
  (when-let [ns-name (some-> (edit/find-namespace zloc)
                             z/next
                             z/next
                             z/string)]
    (when (some-> zloc z/node n/keyword-node?)
      (let [kw (z/string zloc)
            auto-resolved-keyword? (string/starts-with? kw "::")
            slash? (string/includes? kw "/")]
        (cond
          (and (not slash?)
               auto-resolved-keyword?)
          {:status :from-auto-resolve-to-namespace
           :ns-name ns-name
           :kwd (subs (z/string zloc) 2)}

          (and slash?
               (not auto-resolved-keyword?)
               (= ns-name (subs (first (string/split kw #"/")) 1)))
          {:status :from-namespace-to-auto-resolve
           :ns-name ns-name
           :kwd (second (string/split (z/string zloc) #"/"))})))))

(defn ^:private ask-to-replace-all-ns-changes [kwd-usages producer]
  (= "Yes"
     (producer/show-message-request
       producer
       (format "Change all other %s usages of this keyword in this namespace?" (dec (count kwd-usages)))
       :info
       [{:title "Yes"}
        {:title "No"}])))

(defn cycle-keyword-auto-resolve [zloc uri db {:keys [producer]}]
  (when-let [{:keys [status ns-name kwd]} (cycle-keyword-auto-resolve-status zloc)]
    (let [kwd-usages (q/find-keyword-usages-by-keyword db uri (symbol ns-name) kwd)
          zloc-edit-fn (fn [loc]
                         (if (= :from-auto-resolve-to-namespace status)
                           (z/edit-> loc
                                     (z/replace (with-meta (n/keyword-node (keyword ns-name kwd))
                                                           (meta (z/node loc)))))
                           (z/edit-> loc
                                     (z/replace (with-meta (n/keyword-node (keyword kwd) true)
                                                           (meta (z/node loc)))))))]
      (if (and (> (count kwd-usages) 1)
               (ask-to-replace-all-ns-changes kwd-usages producer))
        (mapv (fn [kwd-usage]
                (let [loc (edit/find-at-element zloc kwd-usage)
                      new-loc (zloc-edit-fn loc)]
                  {:range (meta (z/node new-loc))
                   :loc new-loc}))
              kwd-usages)
        (let [new-loc (zloc-edit-fn zloc)]
          [{:range (meta (z/node new-loc))
            :loc new-loc}])))))
