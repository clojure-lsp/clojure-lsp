(ns clojure-lsp.refactor.transform
  (:require
   [clojure-lsp.common-symbols :as common-sym]
   [clojure-lsp.db :as db]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]
   [taoensso.timbre :as log]))

(defn result [zip-edits]
  (mapv (fn [zip-edit]
          (let [loc (:loc zip-edit)]
            (-> zip-edit
                (assoc :new-text (if loc (z/string loc) ""))
                (dissoc :loc))))
        zip-edits))

(defn find-other-colls [zloc]
  (let [sexpr (z/sexpr zloc)]
    (cond
      (map? sexpr) [:vector :set :list]
      (vector? sexpr) [:set :list :map]
      (set? sexpr) [:list :map :vector]
      (list? sexpr) [:map :vector :set])))

(defn change-coll
  "Change collection to specified collection"
  [zloc coll]
  (let [sexpr (z/sexpr zloc)]
    (if (coll? sexpr)
      (let [node (z/node zloc)
            coerce-to-next (fn [_ children]
                             (case (keyword coll)
                               :map (n/map-node children)
                               :vector (n/vector-node children)
                               :set (n/set-node children)
                               :list (n/list-node children)))]
        [{:range (meta node)
          :loc (z/replace zloc (coerce-to-next sexpr (n/children node)))}])
      [])))

(defn cycle-coll
  "Cycles collection between vector, list, map and set"
  [zloc]
  (let [sexpr (z/sexpr zloc)]
    (if (coll? sexpr)
      (let [node (z/node zloc)
            coerce-to-next (fn [sexpr children]
                             (cond
                               (map? sexpr) (n/vector-node children)
                               (vector? sexpr) (n/set-node children)
                               (set? sexpr) (n/list-node children)
                               (list? sexpr) (n/map-node children)))]
        [{:range (meta node)
          :loc (z/replace zloc (coerce-to-next sexpr (n/children node)))}])
      [])))

(defn thread-sym
  [zloc sym top-meta]
  (let [movement (if (= '-> sym) z/right (comp z/rightmost z/right))]
    (if-let [first-loc (-> zloc z/down movement)]
      (let [first-node (z/node first-loc)
            parent-op (z/sexpr (z/left zloc))
            threaded? (= sym parent-op)
            meta-node (cond-> zloc
                        threaded? z/up
                        :always (-> z/node meta))
            first-col (+ (count (str sym)) (:col top-meta))
            result-loc (-> first-loc
                           (z/leftmost)
                           (z/edit->
                             (movement)
                             (z/remove))
                           (z/up)
                           ((fn [loc] (cond-> loc
                                        (edit/single-child? loc)
                                        (-> z/down edit/raise)

                                        threaded?
                                        (-> (z/insert-left first-node)
                                            (z/left)
                                            (z/insert-right* (n/spaces first-col))
                                            (z/insert-right* (n/newlines 1))
                                            z/up)

                                        (not threaded?)
                                        (-> (edit/wrap-around :list)
                                            (z/insert-child (n/spaces first-col))
                                            (z/insert-child (n/newlines 1))
                                            (z/insert-child first-node)
                                            (z/insert-child sym))))))]
        [{:range meta-node
          :loc result-loc}])
      [])))

(def thread-invalid-symbols
  (set/union edit/function-definition-symbols
             '#{-> ->> ns :require :import deftest testing comment when if}))

(defn can-thread? [zloc]
  (and (= (z/tag zloc) :list)
       (not (contains? thread-invalid-symbols
                       (some-> zloc z/next z/sexpr)))))

(defn thread-first
  [zloc]
  (when (can-thread? zloc)
    (thread-sym zloc '-> (meta (z/node zloc)))))

(defn thread-last
  [zloc]
  (when (can-thread? zloc)
    (thread-sym zloc '->> (meta (z/node zloc)))))

(defn thread-all
  [zloc sym]
  (when (can-thread? zloc)
    (let [top-meta (meta (z/node zloc))
          [{top-range :range} :as result] (thread-sym zloc sym top-meta)]
      (loop [[{:keys [loc]} :as result] result]
        (let [next-loc (z/right (z/down loc))]
          (if (and (can-thread? next-loc) (z/right (z/down next-loc)))
            (recur (thread-sym next-loc sym top-meta))
            (assoc-in result [0 :range] top-range)))))))

(defn thread-first-all
  [zloc]
  (thread-all zloc '->))

(defn thread-last-all
  [zloc]
  (thread-all zloc '->>))

(defn unwind-thread
  [zloc]
  (let [thread-loc (if (= (z/tag zloc) :list)
                     (z/next zloc)
                     zloc)
        thread-sym (#{'-> '->> 'some-> 'some->>} (z/sexpr thread-loc))]
    (when thread-sym
      (let [val-loc (z/right thread-loc)
            target-loc (z/right val-loc)
            extra? (z/right target-loc)
            insert-fn (if (string/ends-with? (name thread-sym) "->")
                        z/insert-right
                        (fn [loc node] (-> loc
                                           (z/rightmost)
                                           (z/insert-right node))))]
        (when (and val-loc target-loc)
          (let [result-loc (-> thread-loc
                               z/up
                               (z/subedit->
                                 z/down
                                 z/right
                                 z/remove
                                 z/right
                                 (cond-> (not= :list (z/tag target-loc)) (edit/wrap-around :list))
                                 (z/down)
                                 (insert-fn (z/node val-loc))
                                 (z/up)
                                 (cond-> (not extra?) (edit/raise))))]
            [{:range (meta (z/node (z/up thread-loc)))
              :loc result-loc}]))))))

(defn unwind-all
  [zloc]
  (loop [current (unwind-thread zloc)
         result nil]
    (if current
      (recur (unwind-thread (:loc (first current))) current)
      result)))

(defn find-within [zloc p?]
  (when (z/find (zsub/subzip zloc) z/next p?)
    (z/find zloc z/next p?)))

(defn replace-in-bind-values [first-bind p? replacement]
  (loop [bind first-bind
         marked? false]
    (let [exists? (some-> bind
                          (z/right)
                          (find-within p?))
          bind' (if exists?
                  (-> bind
                      (edit/mark-position-when :first-occurrence (not marked?))
                      (z/edit->
                        (z/right)
                        (find-within p?)
                        (z/replace replacement)))
                  bind)]
      (if-let [next-loc (z/right (z/right bind'))]
        (recur next-loc (or marked? exists?))
        (edit/back-to-mark-or-nil bind' :first-occurrence)))))

(defn find-let-form [zloc]
  (some-> zloc
          (edit/find-ops-up "let")
          z/up))

(defn move-to-let
  "Adds form and symbol to a let further up the tree"
  [zloc binding-name]
  (when-let [let-top-loc (find-let-form zloc)]
    (let [let-loc (z/down (zsub/subzip let-top-loc))
          bound-string (z/string zloc)
          bound-node (z/node zloc)
          binding-sym (symbol binding-name)
          bindings-loc (z/right let-loc)
          {:keys [col]} (meta (z/node bindings-loc)) ;; indentation of bindings
          first-bind (z/down bindings-loc)
          bindings-pos (replace-in-bind-values
                         first-bind
                         #(= bound-string (z/string %))
                         binding-sym)
          with-binding (if bindings-pos
                         (-> bindings-pos
                             (z/insert-left binding-sym)
                             (z/insert-left* bound-node)
                             (z/insert-left* (n/newlines 1))
                             (z/insert-left* (n/spaces col)))
                         (-> bindings-loc
                             (cond->
                              first-bind (z/append-child* (n/newlines 1))
                              first-bind (z/append-child* (n/spaces col))) ; insert let and binding backwards
                             (z/append-child binding-sym) ; add binding symbol
                             (z/append-child bound-node)
                             (z/down)
                             (z/rightmost)))
          new-let-loc (loop [loc (z/next with-binding)]
                        (cond
                          (z/end? loc) (z/replace let-top-loc (z/root loc))
                          (= (z/string loc) bound-string) (recur (z/next (z/replace loc binding-sym)))
                          :else (recur (z/next loc))))]
      [{:range (meta (z/node (z/up let-loc)))
        :loc new-let-loc}])))

(defn introduce-let
  "Adds a let around the current form."
  [zloc binding-name]
  (let [sym (symbol binding-name)
        {:keys [col]} (meta (z/node zloc))
        loc (-> zloc
                (edit/wrap-around :list) ; wrap with new let list
                (z/insert-child 'let) ; add let
                (z/append-child* (n/newlines 1)) ; add new line after location
                (z/append-child* (n/spaces (inc col)))  ; indent body
                (z/append-child sym) ; add new symbol to body of let
                (z/down) ; enter let list
                (z/right) ; skip 'let
                (edit/wrap-around :vector) ; wrap binding vec around form
                (z/insert-child sym) ; add new symbol as binding
                z/up
                (edit/join-let))]
    [{:range (meta (z/node (or loc zloc)))
      :loc loc}]))

(defn expand-let
  "Expand the scope of the next let up the tree."
  [zloc]
  (let [let-loc (some-> zloc
                        (edit/find-ops-up "let")
                        z/up)]
    (when let-loc
      (let [bind-node (-> let-loc z/down z/right z/node)
            parent-loc (edit/parent-let? let-loc)]
        (if parent-loc
          [{:range (meta (z/node parent-loc))
            :loc (edit/join-let let-loc)}]
          (let [{:keys [col] :as parent-meta} (meta (z/node (z/up let-loc)))]
            [{:range parent-meta
              :loc (-> let-loc
                       (z/splice) ; splice in let
                       (z/remove) ; remove let
                       (z/next)
                       (z/remove) ; remove binding
                       (z/find z/up #(= (z/tag %) :list)) ; go to parent form container
                       (edit/wrap-around :list) ; wrap with new let list
                       (z/insert-child* (n/spaces col)) ; insert let and bindings backwards
                       (z/insert-child* (n/newlines 1)) ; insert let and bindings backwards
                       (z/insert-child bind-node)
                       (z/insert-child 'let)
                       (edit/join-let))}]))))))

(defn ^:private sort-by-if-enabled [fn type coll]
  (if (get-in @db/db [:settings :clean :sort type] true)
    (sort-by fn coll)
    coll))

(defn ^:private process-clean-ns
  [ns-loc removed-nodes col ns-inner-blocks-indentation form-type]
  (let [sep (n/whitespace-node (apply str (repeat col " ")))
        single-space (n/whitespace-node " ")
        forms (->> removed-nodes
                   z/node
                   n/children
                   (remove n/printable-only?)
                   (sort-by-if-enabled
                     (comp (fn [sexpr]
                             (if (symbol? sexpr)
                               (str sexpr)
                               (str (first sexpr)))) n/sexpr)
                     form-type)
                   (map-indexed (fn [idx node]
                                  (if (and (= :same-line ns-inner-blocks-indentation)
                                           (= idx 0))
                                    [single-space node]
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
                   (z/replace (n/list-node forms))))))

(defn ^:private remove-unused-refers
  [node unused-refers]
  (let [node-refers (-> node z/down (z/find-next-value ':refer) z/right z/sexpr)
        unused-refers-symbol (->> unused-refers (map (comp symbol name)) set)
        removed-refers (remove unused-refers-symbol node-refers)]
    (if (empty? removed-refers)
      (-> node
          z/down
          (z/find-next-value ':refer)
          z/remove
          z/right
          z/remove)
      (-> node
          z/down
          (z/find-next-value ':refer)
          z/right
          (z/replace (n/vector-node (interpose (n/whitespace-node " ")
                                               (vec (sort-by-if-enabled identity :refer removed-refers)))))
          z/up))))

(defn ^:private remove-unused-require
  [node unused-aliases unused-refers]
  (cond
    (not (z/vector? node))
    node

    (contains? unused-aliases (-> node z/down z/leftmost z/sexpr))
    (z/remove node)

    (= :vector (-> node z/down (z/find-next-value ':refer) z/right z/tag))
    (remove-unused-refers node unused-refers)

    :else
    node))

(defn ^:private remove-unused-requires
  [unused-aliases unused-refers nodes]
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
      (edit/map-children nodes #(remove-unused-require % unused-aliases unused-refers)))))

(defn ^:private clean-requires
  [ns-loc unused-aliases unused-refers ns-inner-blocks-indentation]
  (if-let [require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)]
    (let [col (or (case ns-inner-blocks-indentation
                    :same-line (some-> require-loc z/node meta :end-col)
                    :next-line (some-> require-loc z/node meta :col dec)
                    :keep (some-> require-loc z/right z/node meta :col dec)
                    :else nil)
                  2)
          removed-nodes (->> require-loc
                             z/remove
                             (remove-unused-requires unused-aliases unused-refers))]
      (process-clean-ns ns-loc removed-nodes col ns-inner-blocks-indentation :require))
    ns-loc))

(defn ^:private package-import?
  [node]
  (or (z/vector? node)
      (z/list? node)))

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
  [ns-loc unused-imports ns-inner-blocks-indentation]
  (if-let [import-loc (z/find-value (zsub/subzip ns-loc) z/next :import)]
    (let [col (if import-loc
                (if (= :same-line ns-inner-blocks-indentation)
                  (-> import-loc z/node meta :end-col)
                  (-> import-loc z/node meta :col dec))
                2)
          removed-nodes (-> import-loc
                            z/remove
                            (edit/map-children #(remove-unused-import % unused-imports)))]
      (process-clean-ns ns-loc removed-nodes col ns-inner-blocks-indentation :import))
    ns-loc))

(defn ^:private resolve-ns-inner-blocks-identation [db]
  (or (get-in db [:settings :clean :ns-inner-blocks-indentation])
      (if (get-in db [:settings :keep-require-at-start?])
        :same-line
        :next-line)))

(defn clean-ns
  [zloc uri]
  (let [safe-loc (or zloc (z/of-string (get-in @db/db [:documents uri :text])))
        ns-loc (edit/find-namespace safe-loc)]
    (when ns-loc
      (let [ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation @db/db)
            filename (shared/uri->filename uri)
            unused-aliases (q/find-unused-aliases (:findings @db/db) filename)
            unused-refers (q/find-unused-refers (:findings @db/db) filename)
            unused-imports (q/find-unused-imports (:findings @db/db) filename)
            result-loc (-> ns-loc
                           (clean-requires unused-aliases unused-refers ns-inner-blocks-indentation)
                           (clean-imports unused-imports ns-inner-blocks-indentation))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn ^:private find-missing-alias-require [zloc]
  (let [require-alias (some-> zloc z/sexpr namespace symbol)
        alias->info (->> (q/find-all-aliases (:analysis @db/db))
                         (group-by :alias))
        possibilities (or (some->> (get alias->info require-alias)
                                   (medley/distinct-by (juxt :to))
                                   (map :to))
                          (->> [(get common-sym/common-alias->info require-alias)]
                               (remove nil?)))]
    (when (= 1 (count possibilities))
      (some-> possibilities first name symbol))))

(defn ^:private find-missing-refer-require [zloc]
  (let [refer-to-add (-> zloc z/sexpr symbol)
        ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)]
    (when (not (z/find-value ns-zip z/next refer-to-add))
      (get common-sym/common-refers->info (z/sexpr zloc)))))

(defn find-missing-require [zloc]
  (let [ns-str (some-> zloc z/sexpr namespace)]
    (if ns-str
      (find-missing-alias-require zloc)
      (find-missing-refer-require zloc))))

(defn ^:private find-class-name [zloc]
  (let [sexpr (z/sexpr zloc)
        value (z/string zloc)]
    (cond

      (string/ends-with? value ".")
      (->> value drop-last (string/join "") symbol)

      (namespace sexpr)
      (-> sexpr namespace symbol)

      :else (z/sexpr zloc))))

(defn find-missing-import [zloc]
  (->> zloc
       find-class-name
       (get common-sym/java-util-imports)))

(defn ^:private add-form-to-namespace [zloc form-to-add form-type form-to-check-exists]
  (let [ns-loc (edit/find-namespace zloc)
        ns-zip (zsub/subzip ns-loc)
        cursor-sym (z/sexpr zloc)
        need-to-add? (and (not (z/find-value ns-zip z/next cursor-sym))
                          (not (z/find-value ns-zip z/next form-to-add))
                          (not (z/find-value ns-zip z/next form-to-check-exists)))]
    (when (and form-to-add need-to-add?)
      (let [add-form-type? (not (z/find-value ns-zip z/next form-type))
            form-type-loc (z/find-value (zsub/subzip ns-loc) z/next form-type)
            ns-inner-blocks-indentation (resolve-ns-inner-blocks-identation @db/db)
            col (if form-type-loc
                  (:col (meta (z/node (z/rightmost form-type-loc))))
                  (if (= :same-line ns-inner-blocks-indentation)
                    2
                    5))
            result-loc (z/subedit-> ns-zip
                                    (cond->
                                     add-form-type? (z/append-child (n/newlines 1))
                                     add-form-type? (z/append-child (n/spaces 2))
                                     add-form-type? (z/append-child (list form-type)))
                                    (z/find-value z/next form-type)
                                    (z/up)
                                    (cond->
                                     (or (not add-form-type?)
                                         (= :next-line ns-inner-blocks-indentation)) (z/append-child* (n/newlines 1)))
                                    (z/append-child* (n/spaces (dec col)))
                                    (z/append-child form-to-add))]
        [{:range (meta (z/node result-loc))
          :loc result-loc}]))))

(defn add-import-to-namespace [zloc import-name]
  (add-form-to-namespace zloc (symbol import-name) :import import-name))

(defn add-common-import-to-namespace [zloc]
  (when-let [import-name (find-missing-import zloc)]
    (add-form-to-namespace zloc (symbol import-name) :import import-name)))

(defn add-known-libspec
  [zloc ns-to-add qualified-ns-to-add]
  (when (and qualified-ns-to-add ns-to-add)
    (add-form-to-namespace zloc [qualified-ns-to-add :as ns-to-add] :require ns-to-add)))

(defn ^:private add-missing-alias-ns [zloc]
  (let [require-alias (some-> zloc z/sexpr namespace symbol)
        qualified-ns-to-add (find-missing-alias-require zloc)]
    (add-known-libspec zloc require-alias qualified-ns-to-add)))

(defn ^:private add-missing-refer [zloc]
  (when-let [qualified-ns-to-add (find-missing-refer-require zloc)]
    (let [refer-to-add (-> zloc z/sexpr symbol)
          ns-loc (edit/find-namespace zloc)
          ns-zip (zsub/subzip ns-loc)
          existing-ns-require (z/find-value ns-zip z/next qualified-ns-to-add)
          add-require? (and (not existing-ns-require)
                            (not (z/find-value ns-zip z/next :require)))
          require-loc (z/find-value (zsub/subzip ns-loc) z/next :require)
          col (if require-loc
                (-> require-loc z/rightmost z/node meta :col)
                5)
          result-loc (if existing-ns-require
                       (z/subedit-> ns-loc
                                    (z/find-value z/next qualified-ns-to-add)
                                    (z/find-value z/next ':refer)
                                    z/right
                                    (z/append-child* (n/spaces 1))
                                    (z/append-child (z/sexpr zloc)))
                       (z/subedit-> ns-loc
                                    (cond->
                                     add-require? (z/append-child (n/newlines 1))
                                     add-require? (z/append-child (n/spaces 2))
                                     add-require? (z/append-child (list :require)))
                                    (z/find-value z/next :require)
                                    (z/up)
                                    (z/append-child* (n/newlines 1))
                                    (z/append-child* (n/spaces (dec col)))
                                    (z/append-child [qualified-ns-to-add :refer [refer-to-add]])))]
      [{:range (meta (z/node result-loc))
        :loc result-loc}])))

(defn add-missing-libspec
  [zloc]
  (let [ns-str (some-> zloc z/sexpr namespace)]
    (if ns-str
      (add-missing-alias-ns zloc)
      (add-missing-refer zloc))))

(defn ^:private resolve-best-alias-suggestion
  [ns-str all-aliases drop-core?]
  (if-let [dot-index (string/last-index-of ns-str ".")]
    (let [suggestion (subs ns-str (inc dot-index))]
      (if (and drop-core?
               (= "core" suggestion))
        (resolve-best-alias-suggestion (subs ns-str 0 dot-index) all-aliases drop-core?)
        suggestion))
    ns-str))

(defn ^:private resolve-best-alias-suggestions
  [ns-str all-aliases]
  (let [alias (resolve-best-alias-suggestion ns-str all-aliases true)]
    (if (contains? all-aliases (symbol alias))
      (if-let [dot-index (string/last-index-of ns-str ".")]
        (let [ns-without-alias (subs ns-str 0 dot-index)
              second-alias-suggestion (resolve-best-alias-suggestion ns-without-alias all-aliases false)]
          (if (= second-alias-suggestion alias)
            #{alias}
            (conj #{alias}
                  (str second-alias-suggestion "." alias))))
        #{alias})
      #{alias})))

(defn find-alias-suggestion [zloc]
  (when-let [ns-str (some-> zloc z/sexpr namespace)]
    (let [analysis (:analysis @db/db)
          ns-definitions (q/find-all-ns-definitions analysis)
          all-aliases (->> (q/find-all-aliases analysis)
                           (map :alias)
                           set)]
      (when (contains? ns-definitions (symbol ns-str))
        (->> (resolve-best-alias-suggestions ns-str all-aliases)
             (map (fn [suggestion]
                    {:ns ns-str
                     :alias suggestion})))))))

(defn add-alias-suggestion [zloc chosen-alias]
  (->> (find-alias-suggestion zloc)
       (filter (comp #(= chosen-alias %) :alias))
       (map (fn [{:keys [ns alias]}]
              (let [ns-usages-nodes (parser/find-forms zloc #(and (= :token (z/tag %))
                                                                  (symbol? (z/sexpr %))
                                                                  (= ns (-> % z/sexpr namespace))))]
                (concat (add-known-libspec zloc (symbol alias) (symbol ns))
                        (->> ns-usages-nodes
                             (map (fn [node]
                                    (z/replace node (-> (str alias "/" (-> node z/sexpr name))
                                                        symbol
                                                        n/token-node
                                                        (with-meta (meta (z/node  node)))))))
                             (map (fn [loc]
                                    {:range (meta (z/node loc))
                                     :loc loc})))))))
       flatten))

(defn extract-function
  [zloc uri fn-name]
  (let [{:keys [row col]} (meta (z/node zloc))
        expr-loc (if (not= :token (z/tag zloc))
                   zloc
                   (z/up (edit/find-op zloc)))
        expr-node (z/node expr-loc)
        expr-meta (meta expr-node)
        form-loc (edit/to-top expr-loc)
        {form-row :row form-col :col :as form-pos} (meta (z/node form-loc))
        fn-sym (symbol fn-name)
        used-syms (->> (q/find-local-usages-under-form (:analysis @db/db)
                                                       (shared/uri->filename uri)
                                                       row
                                                       col
                                                       (:end-row expr-meta)
                                                       (:end-col expr-meta))
                       (mapv (comp symbol name :name)))
        expr-edit (-> (z/of-string "")
                      (z/replace `(~fn-sym ~@used-syms)))
        defn-edit (-> (z/of-string "(defn)\n\n")
                      (z/append-child fn-sym)
                      (z/append-child used-syms)
                      (z/append-child* (n/newlines 1))
                      (z/append-child* (n/spaces 2))
                      (z/append-child expr-node))]
    [{:loc defn-edit
      :range (assoc form-pos
               :end-row form-row
               :end-col form-col)}
     {:loc (z/of-string "\n\n")
      :range (assoc form-pos
               :end-row form-row
               :end-col form-col)}
     {:loc expr-edit
      :range expr-meta}]))

(defn find-function-form [zloc]
  (apply edit/find-ops-up zloc (mapv str edit/function-definition-symbols)))

(defn cycle-privacy
  [zloc]
  (when-let [oploc (find-function-form zloc)]
    (let [op (z/sexpr oploc)
          switch-defn-? (and (= 'defn op)
                             (not (get-in @db/db [:settings :use-metadata-for-privacy?])))
          switch-defn? (= 'defn- op)
          name-loc (z/right oploc)
          private? (or switch-defn?
                       (-> name-loc z/sexpr meta :private))
          switch (cond
                   switch-defn? 'defn
                   switch-defn-? 'defn-
                   private? (vary-meta (z/sexpr name-loc) dissoc :private)
                   (not private?) (n/meta-node :private (z/node name-loc)))
          source (if (or switch-defn? switch-defn-?)
                   oploc
                   name-loc)]
      [{:loc (z/replace source switch)
        :range (meta (z/node source))}])))

(defn inline-symbol?
  [{:keys [filename name-row name-col] :as definition}]
  (when definition
    (let [{:keys [text]} (get-in @db/db [:documents (shared/filename->uri filename)])]
      (some-> (parser/loc-at-pos text name-row name-col)
              edit/find-op
              z/sexpr
              #{'let 'def}))))

(defn inline-symbol
  [uri row col]
  (let [definition (q/find-definition-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col)
        references (q/find-references-from-cursor (:analysis @db/db) (shared/uri->filename uri) row col false)
        def-uri (shared/filename->uri (:filename definition))
        def-text (get-in @db/db [:documents def-uri :text])
        def-loc (parser/loc-at-pos def-text (:name-row definition) (:name-col definition))
        op (inline-symbol? definition)]
    (when op
      (let [val-loc (z/right def-loc)
            end-pos (if (= op 'def)
                      (meta (z/node (z/up def-loc)))
                      (meta (z/node val-loc)))
            prev-loc (if (= op 'def)
                       (z/left (z/up def-loc))
                       (z/left def-loc))
            start-pos (if prev-loc
                        (set/rename-keys (meta (z/node prev-loc))
                                         {:end-row :row :end-col :col})
                        (meta (z/node def-loc)))
            def-range {:row (:row start-pos)
                       :col (:col start-pos)
                       :end-row (:end-row end-pos)
                       :end-col (:end-col end-pos)}]
        (reduce
          (fn [accum {:keys [filename] :as element}]
            (update accum
                    (shared/filename->uri filename)
                    (fnil conj [])
                    {:loc val-loc :range element}))
          {def-uri [{:loc nil :range def-range}]}
          references)))))

(defn can-create-function? [zloc]
  (and zloc
       (#{:list :token} (z/tag zloc))))

(defn create-function [zloc]
  (when (can-create-function? zloc)
    (let [fn-form (if (= :token (z/tag zloc))
                    (z/up zloc)
                    zloc)
          fn-name (z/string (z/down fn-form))
          privacy-meta? (get-in @db/db [:settings :use-metadata-for-privacy?] false)
          new-fn-str (if privacy-meta?
                       (format "(defn ^:private %s)" (symbol fn-name))
                       (format "(defn- %s)\n\n" (symbol fn-name)))
          args (->> fn-form
                    z/node
                    n/children
                    (drop 1)
                    (filter (complement n/whitespace?))
                    (map-indexed (fn [index node]
                                   (if (and (= :token (n/tag node))
                                            (symbol? (n/sexpr node)))
                                     (n/sexpr node)
                                     (symbol (str "arg" (inc index))))))
                    vec)
          expr-loc (z/up (edit/find-op zloc))
          form-loc (edit/to-top expr-loc)
          {form-row :row form-col :col :as form-pos} (meta (z/node form-loc))
          defn-edit (-> (z/of-string new-fn-str)
                        (z/append-child* (n/newlines 1))
                        (z/append-child* (n/spaces 2))
                        (z/append-child args)
                        (z/append-child* (n/newlines 1))
                        (z/append-child* (n/spaces 2)))]

      [{:loc defn-edit
        :range (assoc form-pos
                 :end-row form-row
                 :end-col form-col)}
       {:loc (z/of-string "\n\n")
        :range (assoc form-pos
                 :end-row form-row
                 :end-col form-col)}])))
