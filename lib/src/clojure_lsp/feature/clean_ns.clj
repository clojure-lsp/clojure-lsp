(ns clojure-lsp.feature.clean-ns
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(defn ^:private resolve-ns-inner-blocks-identation [db]
  (or (settings/get db [:clean :ns-inner-blocks-indentation])
      (if (settings/get db [:keep-require-at-start?])
        :same-line
        :next-line)))

(defn ^:private sort-by-if-enabled [fn type db coll]
  (if-let [sort-type (settings/get db [:clean :sort type] true)]
    (if (= :lexicographically sort-type)
      (sort-by str coll)
      (sort-by fn coll))
    coll))

(defn ^:private refer-node-with-add-new-lines [nodes]
  (->> (cons (first nodes) nodes)
       (partition 2 1)
       (mapv (fn [[[_ prev] [n curr]]]
               (if (= prev curr) [n false] [n true])))))

(defn ^:private remove-empty-reader-conditional
  [new-node]
  (let [reader-macro? (= :reader-macro (some-> new-node z/up z/up z/tag))
        empty-reader-conditional? (when reader-macro?
                                    (or (<= (-> new-node z/up z/sexpr count) 1)
                                        (and (or (= :vector (z/tag new-node))
                                                 (= :list (z/tag new-node)))
                                             (-> new-node z/sexpr empty?))))]
    (if empty-reader-conditional?
      (-> new-node
          z/up
          z/up
          z/remove)
      new-node)))

(defn ^:private process-clean-ns
  [ns-loc removed-nodes col form-type clean-ctx]
  (let [removed-nodes (edit/map-children removed-nodes remove-empty-reader-conditional)
        sep (n/whitespace-node (apply str (repeat col " ")))
        single-space (n/whitespace-node " ")
        pre-nodes (->> removed-nodes
                       z/node
                       n/children
                       (remove n/whitespace?))
        forms-w-comments (->> pre-nodes
                              (map-indexed
                                (fn [idx node]
                                  (if (some-> (nth pre-nodes (inc idx) nil)
                                              n/comment?)
                                    (n/vector-node [node (nth pre-nodes (inc idx))])
                                    (when (not (n/comment? node))
                                      node))))
                              (remove #(or (nil? %)
                                           (n/printable-only? %)))
                              (sort-by-if-enabled
                                (comp
                                  #(cond
                                     (symbol? %) (string/lower-case %)
                                     (vector? (first %)) (some-> % ffirst string/lower-case)
                                     :else (some-> % first string/lower-case))
                                  n/sexpr)
                                form-type
                                (:db clean-ctx))
                              (map
                                (fn [node]
                                  (if-let [[n comment] (and (= :vector (n/tag node))
                                                            (n/children node))]
                                    (if (and comment (n/comment? comment))
                                      [n comment]
                                      [node])
                                    [node])))
                              (apply concat))
        forms (->> forms-w-comments
                   (map-indexed
                     (fn [idx node]
                       (cond
                         (n/comment? node)
                         [single-space node]

                         (and (= :same-line (:ns-inner-blocks-indentation clean-ctx))
                              (= idx 0))
                         [single-space node]

                         (some-> (nth forms-w-comments (dec idx) nil)
                                 n/comment?)
                         [sep node]

                         :else
                         [(n/newlines 1) sep node])))
                   (apply concat)
                   (cons (n/keyword-node form-type)))]
    (if (empty? (z/child-sexprs removed-nodes))
      (z/subedit-> ns-loc
                   (z/find-value z/next form-type)
                   (z/up)
                   z/remove)
      (z/subedit-> ns-loc
                   (z/find-value z/next form-type)
                   (z/up)
                   (z/replace (with-meta (n/list-node forms)
                                         (-> removed-nodes z/node meta)))))))

(defn ^:private sort-refers-checking-new-lines
  [root-node initial-sep-spaces db nodes]
  (let [sorted-refers (->> nodes
                           (sort-by-if-enabled (comp string/lower-case n/sexpr) :refer db))
        as-alias-before-refer-node (-> root-node z/down (z/find-value z/right ':refer) (z/find-value z/left ':as))
        extra-alias-before-refer-spaces (if as-alias-before-refer-node
                                          (+ (-> as-alias-before-refer-node z/next z/sexpr str count)
                                             2 ;; :as
                                             3 ;; spaces between
                                             )
                                          0)
        init-refer-sep (+ initial-sep-spaces
                          (-> root-node z/down z/sexpr str count)
                          (+ 12 extra-alias-before-refer-spaces))]
    (->> sorted-refers
         (map-indexed (fn [idx refer-node]
                        (let [end-col (->> sorted-refers
                                           (take idx)
                                           (map (comp count str n/sexpr))
                                           (interpose 1)
                                           (reduce + 0)
                                           (+ init-refer-sep (-> refer-node n/sexpr str count)))
                              max-line-length (settings/get db [:clean :sort :refer :max-line-length] 80)]
                          (if (and max-line-length
                                   (> max-line-length 0))
                            (let [lines-n (if (> (quot end-col max-line-length) 0)
                                            (+ init-refer-sep end-col)
                                            end-col)]
                              [refer-node (quot lines-n max-line-length)])
                            [refer-node 0]))))
         refer-node-with-add-new-lines
         (map (fn [[refer-node add-new-line?]]
                (if add-new-line?
                  [refer-node [(n/newlines 1)
                               (n/whitespace-node (string/join "" (repeat (dec (dec init-refer-sep)) " ")))
                               refer-node]]
                  [refer-node [(n/whitespace-node " ") refer-node]])))
         (mapv last)
         (apply concat)
         rest)))

(defn ^:private remove-unused-refers
  [node unused-refers initial-sep-spaces db]
  (let [node-refers (-> node z/down (z/find-next-value ':refer) z/right z/node n/children)
        unused-refers-symbol (->> unused-refers (map (comp symbol name)) set)
        removed-refers (->> node-refers
                            (remove n/printable-only?)
                            (remove (comp unused-refers-symbol n/sexpr)))]
    (if (empty? removed-refers)
      (let [ns-only (-> node
                        z/down
                        (z/find-next-value ':refer)
                        z/remove
                        z/right
                        z/remove)]
        (if (> (count (z/child-sexprs (z/up ns-only))) 1)
          ns-only
          (z/remove (z/up ns-only))))
      (-> node
          z/down
          (z/find-next-value ':refer)
          z/right
          (z/replace (->> removed-refers
                          (sort-refers-checking-new-lines node initial-sep-spaces db)
                          n/vector-node))
          z/up))))

(defn ^:private remove-unused-duplicate-requires
  [node {:keys [filename db]}]
  (if-let [alias (some-> node
                         z/down
                         (z/find-next-value ':as)
                         z/right
                         z/sexpr)]
    (let [local-analysis (get-in @db [:analysis filename])
          used-alias? (some #(and (= :var-usages (:bucket %))
                                  (= alias (:alias %)))
                            local-analysis)]
      (if used-alias?
        node
        (z/remove node)))
    node))

(defn ^:private remove-unused-require
  [node {:keys [unused-aliases unused-refers duplicate-requires db] :as clean-ctx} initial-sep-spaces]
  (let [namespace-expr (some-> node z/down z/leftmost z/sexpr)]
    (cond
      (not (z/vector? node))
      node

      (contains? unused-aliases namespace-expr)
      (z/remove node)

      (= :vector (-> node z/down (z/find-next-value ':refer) z/right z/tag))
      (remove-unused-refers node unused-refers initial-sep-spaces db)

      (contains? duplicate-requires namespace-expr)
      (remove-unused-duplicate-requires node clean-ctx)

      :else
      node)))

(defn ^:private remove-unused-requires
  [nodes {:keys [unused-aliases unused-refers] :as clean-ctx} initial-sep-spaces]
  (let [single-require? (= 1 (count (z/child-sexprs nodes)))
        first-node      (z/next nodes)
        first-node-ns   (when (and single-require?
                                   (z/vector? first-node))
                          (-> first-node z/down z/leftmost z/sexpr))
        first-node-refers (when (and single-require?
                                     (z/vector? first-node)
                                     (-> first-node
                                         z/down
                                         (z/find-next-value ':all)
                                         not))
                            (->> (-> first-node
                                     z/down
                                     (z/find-next-value ':refer)
                                     z/right
                                     z/sexpr)
                                 (map #(symbol (str first-node-ns) (str %)))
                                 set))
        single-unused?  (when (and single-require? (z/vector? first-node))
                          (or (contains? unused-aliases first-node-ns)
                              (and (seq first-node-refers)
                                   (seq unused-refers)
                                   (set/subset? first-node-refers unused-refers))))]
    (if single-unused?
      (z/remove first-node)
      (edit/map-children nodes #(remove-unused-require % clean-ctx initial-sep-spaces)))))

(defn ^:private clean-requires
  [ns-loc clean-ctx]
  (if-let [require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)]
    (let [col (or (case (:ns-inner-blocks-indentation clean-ctx)
                    :same-line (some-> require-loc z/node meta :end-col)
                    :next-line (some-> require-loc z/node meta :col dec)
                    :keep (some-> require-loc z/right z/node meta :col dec)
                    :else nil)
                  2)
          removed-nodes (-> require-loc
                            z/remove
                            (remove-unused-requires clean-ctx col))]
      (process-clean-ns ns-loc removed-nodes col :require clean-ctx))
    ns-loc))

(defn ^:private package-import?
  [node]
  (and (or (z/vector? node)
           (z/list? node))
       (not (or (z/vector? (z/next node))
                (z/list? (z/next node))))))

(defn ^:private remove-unused-package-import
  [node base-package unused-imports]
  (cond
    (string/includes? (z/string node) ".")
    node

    (contains? unused-imports
               (symbol (str base-package "." (z/string node))))
    (z/remove node)

    :else
    node))

(defn ^:private remove-unused-import
  [parent-node unused-imports]
  (cond
    (package-import? parent-node)
    (let [base-package (-> parent-node z/down z/leftmost z/string)
          removed (edit/map-children parent-node
                                     #(remove-unused-package-import % base-package unused-imports))]
      (if (= 1 (count (z/child-sexprs removed)))
        (z/remove removed)
        removed))

    (contains? unused-imports (z/sexpr parent-node))
    (z/remove parent-node)

    :else
    parent-node))

(defn ^:private clean-imports
  [ns-loc {:keys [ns-inner-blocks-indentation unused-imports] :as clean-ctx}]
  (if-let [import-loc (z/find-value (zsub/subzip ns-loc) z/next :import)]
    (let [col (if import-loc
                (if (= :same-line ns-inner-blocks-indentation)
                  (-> import-loc z/node meta :end-col)
                  (-> import-loc z/node meta :col dec))
                2)
          removed-nodes (-> import-loc
                            z/remove
                            (edit/map-children #(remove-unused-import % unused-imports)))]
      (process-clean-ns ns-loc removed-nodes col :import clean-ctx))
    ns-loc))

(defn ^:private sort-ns-children
  [ns-loc settings]
  (if (get-in settings [:clean :sort :ns] true)
    (let [require-loc (-> ns-loc
                          (z/find-next-depth-first #(and (= :list (z/tag %))
                                                         (= :require (z/sexpr (z/next %))))))
          import-loc (-> ns-loc
                         (z/find-next-depth-first #(and (= :list (z/tag %))
                                                        (= :import (z/sexpr (z/next %))))))]
      (if (and require-loc
               import-loc
               (< (-> import-loc z/node meta :row)
                  (-> require-loc z/node meta :row)))
        (z/subedit-> ns-loc
                     (z/find-next-value z/next :import)
                     z/up
                     (z/replace (z/node require-loc))
                     z/right
                     (z/find-next-value z/next :require)
                     z/up
                     (z/replace (z/node import-loc)))
        ns-loc))
    ns-loc))

(defn clean-ns-edits
  [zloc uri db]
  (let [settings (settings/all db)
        ;; TODO: use parser?
        safe-loc (or zloc (z/of-string (get-in @db [:documents uri :text])))
        ns-loc (edit/find-namespace safe-loc)
        analysis (:analysis @db)
        findings (:findings @db)]
    (when ns-loc
      (let [ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation db)
            filename (shared/uri->filename uri)
            unused-aliases* (future (q/find-unused-aliases analysis findings filename))
            unused-refers* (future (q/find-unused-refers analysis findings filename))
            unused-imports* (future (q/find-unused-imports analysis findings filename))
            duplicate-requires* (future (q/find-duplicate-requires findings filename))
            clean-ctx {:db db
                       :filename filename
                       :unused-aliases @unused-aliases*
                       :unused-refers @unused-refers*
                       :unused-imports @unused-imports*
                       :duplicate-requires @duplicate-requires*
                       :ns-inner-blocks-indentation ns-inner-blocks-indentation}
            result-loc (-> ns-loc
                           (clean-requires clean-ctx)
                           (clean-imports clean-ctx)
                           (sort-ns-children settings))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))
