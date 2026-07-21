(ns clojure-lsp.feature.clean-ns
  (:require
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :refer [fast=]]
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
        :keep)))

(defn ^:private sort-by-if-enabled [fn type db coll]
  (if-let [sort-type (settings/get db [:clean :sort type] true)]
    (if (fast= :lexicographically sort-type)
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
  (let [reader-macro? (fast= :reader-macro (some-> new-node z/up z/up z/tag))
        empty-reader-conditional? (when reader-macro?
                                    (or (<= (-> new-node z/up z/sexpr count) 1)
                                        (and (or (fast= :vector (z/tag new-node))
                                                 (fast= :list (z/tag new-node)))
                                             (-> new-node z/sexpr empty?))))]
    (if empty-reader-conditional?
      (-> new-node
          z/up
          z/up
          z/remove)
      new-node)))

(defn ^:private find-after-libspec-comment
  "look for comment to the right of the libspec 'node' on the same line"
  [require-nodes idx libspec-node]
  (when-let [right-node (nth require-nodes (inc idx) nil)]
    (when (and (n/comment? right-node)
               (= (:row (meta right-node)) (:end-row (meta libspec-node))))
      right-node)))

(defn ^:private comment-or-discard? [node]
  (or (n/comment? node) (fast= :uneval (n/tag node))))

(defn ^:private find-interstitial-nodes
  "find any interstitial nodes (comments or discard macros) that are between current libspec idx 
   and the previous libspec (or top of vector)"
  [require-nodes libspec-idx]

  ;; find previous node (or top) 
  (let [prev-node-idx (first (filter #(n/sexpr-able? (nth require-nodes %)) (range (dec libspec-idx) -1 -1)))
        prev-node-row (if prev-node-idx (:end-row (meta (nth require-nodes prev-node-idx))) -1)]

    ;; loop backward until we hit the previous require or top
    (loop [n (dec libspec-idx)
           found-comments []]

      ;; gather comments while between libspecs (or hit top)
      (if (and (not (neg? n))
               (comment-or-discard? (nth require-nodes n))
               (> (some-> (nth require-nodes n) meta :row) prev-node-row))
        (recur (dec n) (concat [(nth require-nodes n)] found-comments))
        found-comments))))

(defn ^:private build-vector-of-associated-nodes
  "returns a vector of a node and associated interstitial nodes (comments and reader discard macros) for temporary
   grouping during sort.  If no associations, just return the node."
  [require-nodes idx node]
  (let [after-node-comment (some-> (find-after-libspec-comment require-nodes idx node)
                                   (vary-meta assoc :associated-node node))
        before-node-comments (find-interstitial-nodes require-nodes idx)]
    (cond
      (and after-node-comment (seq before-node-comments))
      (vary-meta (n/vector-node [node after-node-comment before-node-comments]) assoc :association-types :before-and-after)

      after-node-comment
      (vary-meta (n/vector-node [node after-node-comment]) assoc :association-types :after-only)

      (seq before-node-comments)
      (vary-meta (n/vector-node [node before-node-comments]) assoc :association-types :before-only)

      :else
      node)))

(defn ^:private has-association-vector?
  "if node is a :vector inside a :vector [[]] or a :list inside a :vector [()]
   then this is a comment grouping we created before the sort"
  [node]
  (and (fast= :vector (n/tag node))
       (or (fast= :vector (some-> node n/children first n/tag))
           (fast= :list (some-> node n/children first n/tag)))))

(defn ^:private expand-comment-associations
  "if this node is a vector with associated comments or discard macros, return them as a 
   vector along with the node.  If not, just return the node."
  [node]
  (case (some-> node meta :association-types)
    :before-only (let [[n before-comments] (n/children node)]
                   (concat before-comments [n]))
    :after-only (let [[n comment] (n/children node)]
                  [n comment])
    :before-and-after (let [[n comment before-comments] (n/children node)]
                        (concat before-comments [n comment]))
    [node]))

(defn ^:private process-clean-ns
  [ns-loc remaining-nodes col keep-first-line-spacing form-type clean-ctx]

  (let [nodes-to-sort (edit/map-children remaining-nodes remove-empty-reader-conditional)
        sep (n/whitespace-node (apply str (repeat col " ")))
        single-space (n/whitespace-node " ")

        pre-nodes (->> nodes-to-sort
                       z/node
                       n/children
                       (remove n/whitespace?))

        sorted-nodes (->> pre-nodes
                          (map-indexed
                            (fn [idx node]
                              (when-not (n/printable-only? node)
                                    ;; get all comments
                                (build-vector-of-associated-nodes pre-nodes idx node))))

                          ;; remove commas, whitespace, newlines, discard macros, etc... 
                          (remove #(or (nil? %)
                                       (n/printable-only? %)))

                          (sort-by-if-enabled
                            (fn [node]
                              (let [tag (n/tag node)]
                                (cond
                                  ;; if it is a libspec namespace, sort on it
                                  (identical? :token tag)
                                  (string/lower-case (n/string node))

                                  ;; vector in vector comment grouping created above, sort on the embedded namespace
                                  (has-association-vector? node)
                                  (some-> node n/sexpr ffirst string/lower-case)

                                  ;; if it is just a plain libspec vector or a prefix list, use the first item
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

                          ;; expand any [node, comment] vector groupings that we had created earlier
                          (map
                            (fn [node]
                              (expand-comment-associations node)))

                          (apply concat))

        nodes-w-formatting (->> sorted-nodes
                                (map-indexed
                                  (fn [idx node]

                                    (cond
                                      ;; comment or discard at start of :require, require must start with newline
                                      (and (comment-or-discard? node)
                                           (= idx 0))
                                      [(n/newlines 1) sep node]

                                      ;; if this is an attached comment, just attach it with a space
                                      (and (n/comment? node)
                                           (:associated-node (meta node)))
                                      [single-space node]

                                      ;; if this is an unattached comment, it'll need a newline unless the preceeding node was a comment too
                                      (comment-or-discard? node)
                                      (if (n/comment? (nth sorted-nodes (dec idx)))
                                        [sep node]
                                        [(n/newlines 1) sep node])

                                      ;; first node just gets a single space for same line indentation
                                      (and (fast= :same-line (:ns-inner-blocks-indentation clean-ctx))
                                           (= idx 0))
                                      [single-space node]

                                      ;; first node when keeping the user's indentation
                                      (and (fast= :keep (:ns-inner-blocks-indentation clean-ctx))
                                           keep-first-line-spacing
                                           (= idx 0))
                                      [(n/whitespace-node (apply str (repeat keep-first-line-spacing " ")))
                                       node]

                                      ;; previous comment so no need for newline
                                      (some-> (nth sorted-nodes (dec idx) nil)
                                              n/comment?)
                                      [sep node]

                                      :else
                                      [(n/newlines 1) sep node])))

                                (apply concat)

                                ;; remove grouping metadata that we added before sort
                                (map #(vary-meta % dissoc :associated-node :association-types))

                                (cons (n/keyword-node form-type)))]
    (if (empty? (z/child-sexprs nodes-to-sort))
      (z/subedit-> ns-loc
                   (z/find-value z/next form-type)
                   (z/up)
                   z/remove)
      (z/subedit-> ns-loc
                   (z/find-value z/next form-type)
                   (z/up)
                   (z/replace (with-meta (n/list-node nodes-w-formatting)
                                         (-> nodes-to-sort z/node meta)))))))

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
        init-refer-sep (+ 2 refers-start-col difference-changed-indentation)
        max-line-length (settings/get db [:clean :sort :refer :max-line-length] 80)
        max-chars-per-line (- max-line-length init-refer-sep)]
    (->> sorted-refers
         (reduce
           (if (and max-line-length
                    (> max-line-length 0))
             (fn [{:keys [line-len cur-line] :as acc} refer-node]
               (let [len (count (str (n/sexpr refer-node)))
                     sep-len (if (zero? line-len) 0 1)
                     line-len (+ line-len sep-len len)]
                 (if (< max-chars-per-line line-len)
                   (-> (assoc acc :line-len len)
                       (update :cur-line inc)
                       (update :res conj [refer-node (inc cur-line)]))
                   (-> (assoc acc :line-len line-len)
                       (update :res conj [refer-node cur-line])))))
             (fn [acc refer-node]
               (update acc :res conj [refer-node 0])))
           {:line-len 0
            :cur-line 0
            :res      []})
         :res
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

(defn ^:private remove-same-line-comment [zloc]
  (let [next-right (z/find zloc z/right* #(or (fast= (z/tag %) :comment) (fast= (z/tag %) :newline)))]
    (if (fast= (z/tag next-right) :comment)
      (z/left (z/next (z/remove* next-right)))
      zloc)))

(defn ^:private libspec-attached-node?
  "true if there is something that looks like a libspec namespace to the left of test-loc on this line"
  [test-loc]
  (when (z/left test-loc)
    (and (fast= (:end-row (meta (z/node (z/left test-loc)))) (:row (meta (z/node test-loc))))
         (not= :uneval (z/tag (z/left test-loc))))))

(defn ^:private remove-interstitial-nodes
  "loops left of libspec-loc until it hits the top or another libspec, removing comments 
   and reader discard macros"
  [libspec-loc]

  (loop [prev-loc libspec-loc
         test-loc (z/left* libspec-loc)]
    (cond
        ;; hit the top, the prev location inside vector is the namespace node 
        ;; (everything else was removed or didn't exist)
      (nil? test-loc)
      prev-loc

      ;; comment on a line by itself, or a reader discard macro - remove it.  If we hit
      ;; the top, return the original libspec (go down in the vector because z/remove
      ;; uses z/prev which jumps out out of the vector)
      (or (and (fast= :comment (z/tag test-loc)) (not (libspec-attached-node? test-loc)))
          (fast= :uneval (z/tag test-loc)))
      (if (nil? (z/left* test-loc))
        (-> test-loc
            z/remove*
            z/down)
        (recur test-loc (z/remove* test-loc)))

      (z/whitespace? test-loc)
      (recur test-loc (z/left* test-loc))

      :else
      (z/right test-loc))))

(defn ^:private remove-libspec-and-associated-comments [libspec-zloc]
  (-> libspec-zloc
      (remove-same-line-comment)
      (remove-interstitial-nodes)
      z/remove))

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
        (remove-libspec-and-associated-comments node)))
    node))

(defn ^:private remove-unused-require
  [node initial-sep-spaces {:keys [unused-aliases unused-refers duplicate-requires] :as clean-ctx}]
  (let [namespace-expr (some-> node z/down z/leftmost z/sexpr)]
    (cond
      (not (z/vector? node))
      node

      (contains? unused-aliases namespace-expr)
      (remove-libspec-and-associated-comments node)

      (fast= :vector (-> node z/down (z/find-next-value ':refer) z/right z/tag))
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
      (remove-unused-require first-node initial-sep-spaces clean-ctx)
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
        coll-node-fn (if (fast= :list (z/tag parent-node))
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

                (fast= :next-line (:ns-import-classes-indentation clean-ctx))
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
    (fast= :uneval (z/tag parent-node))
    parent-node

    (package-import? parent-node)
    (let [base-package (-> parent-node z/down z/leftmost z/string)
          removed (edit/map-children parent-node
                                     #(remove-unused-package-import % base-package unused-imports))
          child-exprs (z/child-sexprs removed)
          classes (rest child-exprs)
          remove-whole-package-import (= 1 (count child-exprs))
          node (if remove-whole-package-import
                 (remove-libspec-and-associated-comments removed)
                 removed)]
      (if (and (get-in settings [:clean :sort :import] true)
               (get-in settings [:clean :sort :import-classes] true)
               (not remove-whole-package-import)
               (not (some-> parent-node z/down z/sexpr #{:clj :cljs})) ;; reader conditionals node
               (not= :uneval (some-> parent-node z/up z/next z/tag))
               (not (some-> parent-node z/up z/next z/sexpr #{:clj :cljs})) ;; inside reader conditionals
               (not (some-> parent-node z/up z/up z/next z/sexpr #{:clj :cljs})) ;; list of imports inside reader macros
               (> (count classes) 1))
        (sorting-package-import-classes parent-node clean-ctx import-loc base-package classes settings node)
        node))

    (contains? unused-imports (z/sexpr parent-node))
    (remove-libspec-and-associated-comments parent-node)

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
                          (z/find-next-depth-first #(and (fast= :list (z/tag %))
                                                         (fast= :require (z/sexpr (z/next %))))))
          import-loc (-> ns-loc
                         (z/find-next-depth-first #(and (fast= :list (z/tag %))
                                                        (fast= :import (z/sexpr (z/next %))))))]
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
        safe-loc (or zloc (parser/zloc-of-file db uri))
        ns-loc (edit/find-namespace safe-loc)]
    (if ns-loc
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
          :loc result-loc}])
      {:no-op? true})))
