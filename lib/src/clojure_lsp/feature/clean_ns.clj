(ns clojure-lsp.feature.clean-ns
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]))

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
  [ns-loc removed-nodes col keep-first-line-spacing form-type clean-ctx]
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
                                (fn [node]
                                  (let [tag (n/tag node)]
                                    (cond
                                      (identical? :token tag)
                                      (string/lower-case (n/string node))

                                      (and (identical? :vector tag)
                                           (identical? :vector (some-> node n/children first n/tag)))
                                      (some-> node n/sexpr ffirst string/lower-case)

                                      (and (or (identical? :vector tag)
                                               (identical? :list tag))
                                           (first (n/children node)))
                                      (-> node n/children first n/string string/lower-case)

                                      ;; we should move cljc reader conditionals before normal requires,
                                      ;; returning 0 we make sure those come before normal requires (because of ascii table),
                                      ;; is there a better way to do this?
                                      ;;
                                      ;; #1057 There are multiple ways to sort a reader conditional, so we keep the user order,
                                      ;; but just move them to top
                                      (identical? :reader-macro tag)
                                      "0"

                                      :else
                                      (some-> node n/sexpr first string/lower-case))))
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

                         (and (= :keep (:ns-inner-blocks-indentation clean-ctx))
                              keep-first-line-spacing
                              (= idx 0))
                         [(n/whitespace-node (apply str (repeat keep-first-line-spacing " ")))
                          node]

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
  [root-node initial-sep-spaces {:keys [db]} nodes]
  (let [sorted-refers (->> nodes
                           (sort-by-if-enabled (comp string/lower-case n/sexpr) :refer db))
        {old-ns-row :row old-ns-col :col} (-> root-node z/next z/node meta)
        new-initial-ns-pos (+ 2 initial-sep-spaces)
        {refers-start-row :row
         refers-start-col :col} (-> root-node
                                    z/down
                                    (z/find-value z/right ':refer)
                                    z/next
                                    z/node
                                    meta)
        difference-changed-indentation (if (= old-ns-row refers-start-row)
                                         (- new-initial-ns-pos old-ns-col)
                                         0)
        init-refer-sep (+ 2 refers-start-col difference-changed-indentation)]
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
  [node unused-refers initial-sep-spaces clean-ctx]
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
                          (sort-refers-checking-new-lines node initial-sep-spaces clean-ctx)
                          n/vector-node))
          z/up))))

(defn ^:private remove-unused-duplicate-requires
  [node {:keys [uri db]}]
  (if-let [alias (some-> node
                         z/down
                         (z/find-next-value ':as)
                         z/right
                         z/sexpr)]
    (let [local-var-usages (get-in db [:analysis uri :var-usages])
          used-alias? (some #(= alias (:alias %))
                            local-var-usages)]
      (if used-alias?
        node
        (z/remove node)))
    node))

(defn ^:private remove-unused-require
  [node initial-sep-spaces {:keys [unused-aliases unused-refers duplicate-requires] :as clean-ctx}]
  (let [namespace-expr (some-> node z/down z/leftmost z/sexpr)]
    (cond
      (not (z/vector? node))
      node

      (contains? unused-aliases namespace-expr)
      (z/remove node)

      (= :vector (-> node z/down (z/find-next-value ':refer) z/right z/tag))
      (remove-unused-refers node unused-refers initial-sep-spaces clean-ctx)

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
      (edit/map-children nodes #(remove-unused-require % initial-sep-spaces clean-ctx)))))

(defn ^:private ns-inner-blocks-indentation-parent-col [parent-loc clean-ctx]
  (or (case (:ns-inner-blocks-indentation clean-ctx)
        :same-line (some-> parent-loc z/node meta :end-col)
        :next-line (some-> parent-loc z/node meta :col dec)
        :keep (some-> parent-loc z/right z/node meta :col dec)
        :else nil)
      2))

(defn ^:private calc-keep-first-line-spacing
  "Difference between end of :require/:import and start of next node,
   nil if they are not on the same line."
  [form-type-loc]
  (let [require-meta (-> form-type-loc z/node meta)
        right-meta (-> form-type-loc z/right z/node meta)]
    (when (= (:row require-meta) (:row right-meta))
      (- (:col right-meta) (:end-col require-meta)))))

(defn ^:private clean-requires
  [ns-loc clean-ctx]
  (if-let [require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)]
    (let [col (ns-inner-blocks-indentation-parent-col require-loc clean-ctx)
          keep-first-line-spacing (calc-keep-first-line-spacing require-loc)
          removed-nodes (-> require-loc
                            z/remove
                            (remove-unused-requires clean-ctx col))]
      (process-clean-ns ns-loc removed-nodes col keep-first-line-spacing :require clean-ctx))
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

(defn ^:private sorting-package-import-classes
  [parent-node clean-ctx import-loc base-package classes settings node]
  (let [parent-node-col (ns-inner-blocks-indentation-parent-col import-loc clean-ctx)
        coll-node-fn (if (= :list (z/tag parent-node))
                       n/list-node
                       n/vector-node)
        sorted-classes (sort-by-if-enabled identity :import-classes (:db clean-ctx) classes)
        classes-per-line (get-in settings [:clean :sort :import-classes :classes-per-line] 3)
        move-classes? (and (not= -1 classes-per-line)
                           (> (count sorted-classes) classes-per-line))
        nodes (cond
                (not move-classes?)
                (->> sorted-classes
                     (mapv (fn [n]
                             [(n/spaces 1) (n/token-node n)]))
                     (concat [(n/token-node (symbol base-package))])
                     flatten)

                (= :next-line (:ns-import-classes-indentation clean-ctx))
                (->> sorted-classes
                     (mapv (fn [n]
                             [(n/newlines 1)
                              (n/spaces (inc parent-node-col))
                              (n/token-node n)]))
                     (concat [(n/token-node (symbol base-package))])
                     flatten)

                :else
                (->> (rest sorted-classes)
                     (mapv (fn [n]
                             [(n/newlines 1)
                              (n/spaces (-> parent-node z/down z/leftmost z/node meta :end-col))
                              (n/token-node n)]))
                     (concat [(n/token-node (symbol base-package))
                              (n/spaces 1)
                              (n/token-node (first sorted-classes))])
                     flatten))]
    (z/replace node (coll-node-fn nodes))))

(defn ^:private remove-unused-import
  [parent-node import-loc unused-imports clean-ctx settings]
  (cond
    (package-import? parent-node)
    (let [base-package (-> parent-node z/down z/leftmost z/string)
          removed (edit/map-children parent-node
                                     #(remove-unused-package-import % base-package unused-imports))
          child-exprs (z/child-sexprs removed)
          classes (rest child-exprs)
          remove-whole-package-import (= 1 (count child-exprs))
          node (if remove-whole-package-import
                 (z/remove removed)
                 removed)]
      (if (and (get-in settings [:clean :sort :import] true)
               (get-in settings [:clean :sort :import-classes] true)
               (not remove-whole-package-import)
               (not (some-> parent-node z/down z/sexpr #{:clj :cljs})) ;; reader conditionals node
               (not (some-> parent-node z/up z/next z/sexpr #{:clj :cljs})) ;; inside reader conditionals
               (not (some-> parent-node z/up z/up z/next z/sexpr #{:clj :cljs})) ;; list of imports inside reader macros
               (> (count classes) 1))
        (sorting-package-import-classes parent-node clean-ctx import-loc base-package classes settings node)
        node))

    (contains? unused-imports (z/sexpr parent-node))
    (z/remove parent-node)

    :else
    parent-node))

(defn ^:private clean-imports
  [ns-loc {:keys [unused-imports] :as clean-ctx} settings]
  (if-let [import-loc (z/find-value (zsub/subzip ns-loc) z/next :import)]
    (let [col (ns-inner-blocks-indentation-parent-col import-loc clean-ctx)
          keep-first-line-spacing (calc-keep-first-line-spacing import-loc)
          removed-nodes (-> import-loc
                            z/remove
                            (edit/map-children #(remove-unused-import % import-loc unused-imports clean-ctx settings)))]
      (process-clean-ns ns-loc removed-nodes col keep-first-line-spacing :import clean-ctx))
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
        safe-loc (or zloc (z/of-string (get-in db [:documents uri :text])))
        ns-loc (edit/find-namespace safe-loc)]
    (when ns-loc
      (let [ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation db)
            unused-aliases* (future (q/find-unused-aliases db uri))
            unused-refers* (future (q/find-unused-refers db uri))
            unused-imports* (future (q/find-unused-imports db uri))
            duplicate-requires* (future (q/find-duplicate-requires db uri))
            clean-ctx {:db db
                       :old-ns-loc ns-loc
                       :uri uri
                       :unused-aliases @unused-aliases*
                       :unused-refers @unused-refers*
                       :unused-imports @unused-imports*
                       :duplicate-requires @duplicate-requires*
                       :ns-inner-blocks-indentation ns-inner-blocks-indentation
                       :ns-import-classes-indentation (settings/get db [:clean :ns-import-classes-indentation] :next-line)}
            result-loc (-> ns-loc
                           (clean-requires clean-ctx)
                           (clean-imports clean-ctx settings)
                           (sort-ns-children settings))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))
