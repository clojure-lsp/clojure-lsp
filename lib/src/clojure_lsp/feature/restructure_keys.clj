(ns clojure-lsp.feature.restructure-keys
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private map-to-restructure [zloc uri db]
  (when zloc
    (let [filename (shared/uri->filename uri)
          {:keys [row col]} (meta (z/node zloc))
          def-elem (q/find-definition-from-cursor db filename row col)
          loc (or (when (and def-elem
                             (= :locals (:bucket def-elem)))
                    (when-let [up-loc (-> zloc
                                          edit/to-top
                                          (edit/find-at-pos (:row def-elem) (:col def-elem))
                                          z/up)]
                      (cond-> up-loc
                        (= :vector (z/tag up-loc)) z/up)))
                  zloc)]
      (when (seq (q/find-locals-under-form db filename (meta (z/node loc))))
        (cond
          (z/map? loc) {:filename filename
                        :replace-loc loc
                        :map-loc loc}
          (z/namespaced-map? loc) (let [qualifier-loc (z/down loc)
                                        {:keys [auto-resolved? prefix]} (z/node qualifier-loc)]
                                    {:filename filename
                                     :replace-loc loc
                                     :map-loc (z/right qualifier-loc)
                                     :map-auto-resolved? auto-resolved?
                                     :map-ns prefix})
          :else nil)))))

(defn can-restructure-keys? [zloc uri db]
  (boolean (map-to-restructure zloc uri db)))

(defn ^:private z-children-seq [loc]
  (->> loc
       z/down
       (iterate z/right)
       (take-while (complement z/end?))))

(defn ^:private z-of-list [children]
  (reduce z/append-child
          (z/of-node (n/list-node []))
          children))

(defn ^:private z-of-map [map-entries]
  (reduce (fn [loc [k v]]
            (z/assoc loc k v))
          (z/of-node (n/map-node []))
          map-entries))

(defn ^:private z-of-symbol [element-name]
  (z/of-node (n/token-node element-name)))

(defn ^:private loc-kw-name [key-loc]
  (let [key-sexpr (z/sexpr key-loc)]
    (when (keyword? key-sexpr)
      (name key-sexpr))))

(defn ^:private reference-elems [local-node db filename]
  (let [local-meta (meta local-node)
        elem (q/find-local-under-cursor db filename (:row local-meta) (:col local-meta))]
    (q/find-references db elem false)))

(defn ^:private restructure-data [key-loc val-loc
                                  db filename
                                  {:keys [map-ns map-auto-resolved?]}]
  (let [key-sexpr (z/sexpr key-loc)]
    (cond
      ;; {:keys [a]} and all its variations
      (and (keyword? key-sexpr)
           (contains? #{"keys" "syms"} (name key-sexpr)))
      (let [k-node (z/node key-loc)
            k-ns (namespace (:k k-node))
            ;; #:prefix{:_/keys [a]} -> (:a element)
            ignore-implications? (= k-ns "_")
            implied-ns (when-not ignore-implications?
                         (or k-ns map-ns))
            implied-auto-resolved? (when-not ignore-implications?
                                     (or (:auto-resolved? k-node)
                                         map-auto-resolved?))
            replace-with (case (name key-sexpr)
                           "keys"
                           (fn [local-node]
                             (let [local-sexpr (n/sexpr local-node)]
                               (cond
                                 ;; {:keys [::a]}            -> (::a element)
                                 ;; {:keys [::my-alias/a]}   -> (::my-alias/a element)
                                 (:auto-resolved? local-node)   local-node
                                 ;; {:keys [my-ns/a]}        -> (:my-ns/a element)
                                 ;; {:keys [:my-ns/a]}       -> (:my-ns/a element)
                                 (qualified-ident? local-sexpr) (n/keyword-node (keyword local-sexpr))
                                 ;; {:my-ns/keys [a]}        -> (:my-ns/a element)
                                 ;; #:my-ns{:keys [a]}       -> (:my-ns/a element)
                                 ;; {::my-alias/keys [a]}    -> (::my-alias/a element)
                                 ;; #::my-alias{:keys [a]}   -> (::my-alias/a element)
                                 implied-ns                     (n/keyword-node (keyword implied-ns (name local-sexpr)) implied-auto-resolved?)
                                 ;; {::keys [a]}             -> (::a element)
                                 ;; #::{:keys [a]}           -> (::a element)
                                 implied-auto-resolved?         (n/keyword-node (keyword local-sexpr) true)
                                 ;; {:keys [a]}              -> (:a element)
                                 :else                          (n/keyword-node (keyword local-sexpr)))))
                           "syms"
                           (fn [local-node]
                             (let [local-sexpr (n/sexpr local-node)]
                               (n/quote-node
                                 (cond
                                   ;; {:syms [my-ns/a]}        -> ('my-ns/a element)
                                   ;; {:syms [:my-ns/a]}       -> ('my-ns/a element)
                                   (qualified-ident? local-sexpr) (symbol local-sexpr)
                                   ;; {:my-ns/syms [a]}        -> ('my-ns/a element)
                                   ;; #:my-ns{:syms [a]}       -> ('my-ns/a element)
                                   implied-ns                     (symbol implied-ns (name local-sexpr))
                                   ;; {:syms [a]}              -> ('a element)
                                   :else                          (symbol local-sexpr))))))]
        (->> val-loc
             z-children-seq
             (map z/node)
             (map (fn [local-node]
                    {:restructure? true
                     :replace-with (replace-with local-node)
                     :reference-elems (reference-elems local-node db filename)}))))
      ;; {a :a}
      (symbol? key-sexpr)
      [{:restructure? true
        :replace-with (z/node val-loc)
        :reference-elems (reference-elems (z/node key-loc) db filename)}]
      ;; {{:keys [a1]} :a}
      :else
      [{:restructure? false
        :keep-key (z/node key-loc)
        :keep-val (z/node val-loc)}])))

(defn ^:private restructure-edits
  [{:keys [replace-with reference-elems]} element-name default-values]
  (let [replacement (z-of-list [replace-with element-name])]
    (map (fn [reference-elem]
           {:range reference-elem
            :loc (if-let [default-val (get default-values (:name reference-elem))]
                   (z-of-list ['get element-name replace-with default-val])
                   replacement)})
         reference-elems)))

(defn restructure-keys [zloc uri db]
  (when-let [{:keys [filename map-loc replace-loc], :as restructure-config}
             (map-to-restructure zloc uri db)]
    (let [map-entry-locs (->> map-loc
                              z-children-seq
                              (partition 2))
          provided-as (->> map-entry-locs
                           (filter (fn [[key-loc _]] (= "as" (loc-kw-name key-loc))))
                           (map (fn [[_ val-loc]]
                                  (z/sexpr val-loc)))
                           first)
          element-name (or provided-as 'element)
          default-values (->> map-entry-locs
                              (filter (fn [[key-loc _]] (= "or" (loc-kw-name key-loc))))
                              (map (fn [[_ val-loc]]
                                     (z/sexpr val-loc)))
                              first)
          restructure-data (->> map-entry-locs
                                (remove (fn [[key-loc _]]
                                          (contains? #{"as" "or"} (loc-kw-name key-loc))))
                                (mapcat (fn [[key-loc val-loc]]
                                          (restructure-data key-loc val-loc
                                                            db filename
                                                            restructure-config))))
          restructurable-data (filter :restructure? restructure-data)
          unrestructurable-data (remove :restructure? restructure-data)]
      (when-let [restructure-edits (->> restructurable-data
                                        (mapcat #(restructure-edits % element-name default-values))
                                        seq)]
        (conj restructure-edits
              (if (seq unrestructurable-data)
                ;; keep map, removing as much of it as possible, and adding element-name
                {:range (meta (z/node map-loc)) ;; not replace-loc, to preserve namespaced maps
                 :loc (let [map-entries (mapv (juxt :keep-key :keep-val)
                                              unrestructurable-data)
                            remaining-default-values (->> restructurable-data
                                                          (mapcat :reference-elems)
                                                          (map :name)
                                                          distinct
                                                          (reduce dissoc default-values))
                            map-entries (cond-> map-entries
                                          (seq remaining-default-values)
                                          (conj [:or remaining-default-values]))]
                        (z-of-map (conj map-entries [:as element-name])))}
                ;; replace map with element-name
                {:range (meta (z/node replace-loc))
                 :loc (z-of-symbol element-name)}))))))
