(ns clojure-lsp.refactor.transform
  (:require
   [clojure-lsp.feature.add-missing-libspec :as f.add-missing-libspec]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.producer :as producer]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as string]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]
   [rewrite-clj.zip.subedit :as zsub]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

(def common-var-definition-symbols
  '#{defn
     defn-
     def
     defmacro
     defmulti
     defmethod
     defonce
     deftest
     deftype
     defrecord})

(defn result [zip-edits]
  (mapv (fn [zip-edit]
          (let [loc (:loc zip-edit)]
            (-> zip-edit
                (assoc :new-text (if loc (z/string loc) ""))
                (dissoc :loc))))
        zip-edits))

(defn find-other-colls [zloc]
  (when (z/sexpr-able? zloc)
    (cond
      (z/map? zloc) [:vector :set :list]
      (z/vector? zloc) [:set :list :map]
      (z/set? zloc) [:list :map :vector]
      (z/list? zloc) [:map :vector :set])))

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

(defn ^:private thread-sym
  [zloc sym top-meta db]
  (let [keep-parens-when-threading? (settings/get db [:keep-parens-when-threading?] false)
        movement (if (= '-> sym) z/right (comp z/rightmost z/right))]
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
                                        (and (edit/single-child? loc)
                                             (not keep-parens-when-threading?))
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
  (set/union common-var-definition-symbols
             '#{-> ->> ns :require :import deftest testing comment when if}))

(defn can-thread-list? [zloc]
  (and (= (z/tag zloc) :list)
       (not (contains? thread-invalid-symbols
                       (some-> zloc z/next z/sexpr)))))

(defn can-thread? [zloc]
  (or (can-thread-list? zloc)
      (and (= (z/tag zloc) :token)
           (= (z/tag (z/up zloc)) :list)
           (not (contains? thread-invalid-symbols
                           (some-> zloc z/up z/next z/sexpr))))))

(defn thread-first
  [zloc db]
  (when (can-thread? zloc)
    (thread-sym zloc '-> (meta (z/node zloc)) db)))

(defn thread-last
  [zloc db]
  (when (can-thread? zloc)
    (thread-sym zloc '->> (meta (z/node zloc)) db)))

(defn thread-all
  [zloc sym db]
  (when (can-thread? zloc)
    (let [zloc (if (= (z/tag zloc) :list) zloc (z/up zloc))
          top-meta (meta (z/node zloc))
          [{top-range :range} :as result] (thread-sym zloc sym top-meta db)]
      (loop [[{:keys [loc]} :as result] result]
        (let [next-loc (z/right (z/down loc))]
          (if (and (can-thread-list? next-loc) (z/right (z/down next-loc)))
            (recur (thread-sym next-loc sym top-meta db))
            (assoc-in result [0 :range] top-range)))))))

(defn thread-first-all
  [zloc db]
  (thread-all zloc '-> db))

(defn thread-last-all
  [zloc db]
  (thread-all zloc '->> db))

(def thread-first-symbols #{'-> 'some->})

(def thread-symbols (set/union thread-first-symbols
                               #{'->> 'some->>}))

(defn can-unwind-thread? [zloc]
  (let [thread-loc (apply edit/find-ops-up zloc (map str thread-symbols))
        thread-sym (when thread-loc
                     (thread-symbols
                       (z/sexpr thread-loc)))]
    (when thread-sym
      {:thread-loc thread-loc
       :thread-sym thread-sym})))

(defn unwind-thread
  [zloc]
  (when-let [{:keys [thread-loc thread-sym]} (can-unwind-thread? zloc)]
    (let [val-loc (z/right thread-loc)
          target-loc (z/right val-loc)
          extra? (z/right target-loc)
          insert-fn (if (some #(string/ends-with? (name thread-sym) (str %))
                              thread-first-symbols)
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
            :loc result-loc}])))))

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
          (let [{:keys [col] :as parent-meta} (meta (z/node (z/up let-loc)))
                result-loc (-> let-loc
                               (z/insert-child ::dummy) ; prepend dummy element to let form
                               (z/splice) ; splice in let
                               (z/right)
                               (z/remove) ; remove let
                               (z/right)
                               (z/remove) ; remove binding
                               (z/find z/up #(= (z/tag %) :list)) ; go to parent form container
                               (z/edit->
                                 (z/find-value z/next ::dummy)
                                 (z/remove)) ; remove dummy element
                               (edit/wrap-around :list) ; wrap with new let list
                               (z/insert-child* (n/spaces col)) ; insert let and bindings backwards
                               (z/insert-child* (n/newlines 1)) ; insert let and bindings backwards
                               (z/insert-child bind-node)
                               (z/insert-child 'let))
                merge-result-with-parent-let? (edit/parent-let? result-loc)]
            [{:range (if merge-result-with-parent-let?
                       (meta (z/node (z/up (z/up let-loc))))
                       parent-meta)
              :loc (edit/join-let result-loc)}]))))))

(defn extract-function
  [zloc uri fn-name db]
  (let [{:keys [row col]} (meta (z/node zloc))
        expr-loc (if (not= :token (z/tag zloc))
                   zloc
                   (z/up (edit/find-op zloc)))
        expr-node (z/node expr-loc)
        expr-meta (meta expr-node)
        form-loc (edit/to-top expr-loc)
        {form-row :row
         form-col :col} (meta (z/node form-loc))
        prev-end-row-w-space (some-> (z/find-next form-loc z/prev #(and (edit/top? %)
                                                                        (z/sexpr-able? %)))
                                     z/node
                                     meta
                                     :end-row
                                     inc)
        fn-sym (symbol fn-name)
        used-syms (->> (q/find-local-usages-under-form (:analysis @db)
                                                       (shared/uri->filename uri)
                                                       row
                                                       col
                                                       (:end-row expr-meta)
                                                       (:end-col expr-meta))
                       (mapv (comp symbol name :name)))
        expr-edit (-> (z/of-string "")
                      (z/replace `(~fn-sym ~@used-syms)))
        defn-edit (-> (z/of-string "\n(defn)\n")
                      (z/append-child fn-sym)
                      (z/append-child used-syms)
                      (z/append-child* (n/newlines 1))
                      (z/append-child* (n/spaces 2))
                      (z/append-child expr-node)
                      z/up)]
    [{:loc defn-edit
      :range {:row (or prev-end-row-w-space form-row)
              :col form-col
              :end-row (or prev-end-row-w-space form-row)
              :end-col form-col}}
     {:loc expr-edit
      :range expr-meta}]))

(defn find-function-form [zloc]
  (apply edit/find-ops-up zloc (mapv str common-var-definition-symbols)))

(defn cycle-privacy
  [zloc db]
  (when-let [oploc (find-function-form zloc)]
    (let [op (z/sexpr oploc)
          switch-defn-? (and (= 'defn op)
                             (not (settings/get db [:use-metadata-for-privacy?])))
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
  [{:keys [filename name-row name-col] :as definition} db]
  (when definition
    (let [{:keys [text]} (get-in @db [:documents (shared/filename->uri filename db)])]
      (some-> (parser/loc-at-pos text name-row name-col)
              edit/find-op
              z/sexpr
              #{'let 'def}))))

(defn inline-symbol
  [uri row col db]
  (let [definition (q/find-definition-from-cursor (:analysis @db) (shared/uri->filename uri) row col db)
        references (q/find-references-from-cursor (:analysis @db) (shared/uri->filename uri) row col false db)
        def-uri (shared/filename->uri (:filename definition) db)
        def-text (get-in @db [:documents def-uri :text])
        def-loc (parser/loc-at-pos def-text (:name-row definition) (:name-col definition))
        op (inline-symbol? definition db)]
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
        {:changes-by-uri
         (reduce
           (fn [accum {:keys [filename] :as element}]
             (update accum
                     (shared/filename->uri filename db)
                     (fnil conj [])
                     {:loc val-loc :range element}))
           {def-uri [{:loc nil :range def-range}]}
           references)}))))

(defn can-create-private-function? [zloc]
  (and zloc
       (#{:list :token} (z/tag zloc))))

(defn can-create-public-function? [zloc uri db]
  (when (and zloc
             (#{:list :token} (z/tag zloc)))
    (if-let [ns-usage (some->> (z/sexpr zloc)
                               namespace
                               symbol
                               (q/find-namespace-usage-by-alias (:analysis @db) (shared/uri->filename uri)))]
      (if-let [ns-def (q/find-definition (:analysis @db) ns-usage db)]
        (when-not (shared/external-filename? (:filename ns-def) (settings/get db [:source-paths]))
          {:ns (:name ns-def)
           :name (-> zloc z/sexpr name)})
        (when-not (shared/external-filename? (:filename ns-usage) (settings/get db [:source-paths]))
          {:new-ns (:name ns-usage)
           :name (-> zloc z/sexpr name)}))
      {:new-ns (-> zloc z/sexpr namespace)
       :name (-> zloc z/sexpr name)})))

(def ^:private thread-first-macro-symbols '#{-> some->})
(def ^:private thread-last-macro-symbols '#{->> some->>})
(def ^:private thread-macro-symbols '#{-> ->> some-> some->>})

(defn ^:private create-function-arg [node index]
  (if (and (= :token (n/tag node))
           (symbol? (n/sexpr node)))
    (n/sexpr node)
    (symbol (str "arg" (inc index)))))

(defn ^:private create-function-for-alias
  [local-zloc ns-or-alias fn-name defn-edit uri db]
  (let [ns-usage (q/find-namespace-usage-by-alias (:analysis @db) (shared/uri->filename uri) (symbol ns-or-alias))
        ns-definition (when ns-usage
                        (q/find-definition (:analysis @db) ns-usage db))
        source-paths (settings/get db [:source-paths])
        def-uri (cond
                  ns-definition
                  (shared/filename->uri (:filename ns-definition) db)
                  ns-usage
                  (shared/namespace->uri (:name ns-usage) source-paths (:filename ns-usage) db)
                  :else
                  (shared/namespace->uri ns-or-alias source-paths (shared/uri->filename uri) db))
        min-range {:row 1 :end-row 1 :col 1 :end-col 1}
        max-range {:row 999999 :end-row 999999 :col 1 :end-col 1}]
    {:show-document-after-edit {:uri def-uri
                                :take-focus? true
                                :range max-range}
     :resource-changes (when-not ns-definition
                         [{:kind "create"
                           :uri def-uri
                           :options {:overwrite? false
                                     :ignore-if-exists? true}}])
     :changes-by-uri {uri (when-not ns-definition
                            (f.add-missing-libspec/add-known-alias
                              local-zloc
                              (symbol ns-or-alias)
                              (symbol fn-name)
                              db))
                      def-uri (->> [(when-not ns-definition
                                      {:loc (z/up (z/of-string (format "(ns %s)\n" ns-or-alias)))
                                       :range min-range})
                                    {:loc defn-edit
                                     :range max-range}
                                    {:loc (z/of-string "\n")
                                     :range max-range}]
                                   (remove nil?))}}))

(defn create-function [local-zloc uri db]
  (when (or (can-create-private-function? local-zloc)
            (can-create-public-function? local-zloc uri db))
    (let [token? (= :token (z/tag local-zloc))
          sexpr (if token?
                  (z/sexpr (z/down (z/up local-zloc)))
                  (z/sexpr local-zloc))
          ns-or-alias (when (qualified-symbol? sexpr) (namespace sexpr))
          thread? (thread-macro-symbols sexpr)
          inside-thread-first? (and (z/leftmost? local-zloc)
                                    (thread-first-macro-symbols (z/sexpr (z/down (z/up (z/up local-zloc))))))
          inside-thread-last? (and (z/leftmost? local-zloc)
                                   (thread-last-macro-symbols (z/sexpr (z/down (z/up (z/up local-zloc))))))
          fn-form (if token? (z/up local-zloc) local-zloc)
          fn-name (cond
                    (and thread? token?) (z/sexpr local-zloc)
                    thread? (z/sexpr (z/down fn-form))
                    :else (z/sexpr (z/down fn-form)))
          fn-name (if ns-or-alias (name fn-name) fn-name)
          new-fn-str (cond
                       ns-or-alias
                       (format "(defn %s)" fn-name)

                       (settings/get db [:use-metadata-for-privacy?] false)
                       (format "(defn ^:private %s)" fn-name)

                       :else
                       (format "(defn- %s)" fn-name))
          args (->> fn-form
                    z/node
                    n/children
                    (drop 1)
                    (remove n/whitespace?)
                    (map-indexed (fn [index node]
                                   (create-function-arg node index)))
                    vec)
          args (cond
                 thread? (pop args)
                 inside-thread-first? (->> args
                                           (cons (create-function-arg (z/node (z/left (z/up local-zloc))) -1))
                                           vec)
                 inside-thread-last? (-> args
                                         (conj (create-function-arg (z/node (z/left (z/up local-zloc))) (count args)))
                                         vec)
                 :else args)
          defn-edit (-> (z/of-string new-fn-str)
                        (z/append-child* (n/spaces 1))
                        (z/append-child args)
                        (z/append-child* (n/newlines 1))
                        (z/append-child* (n/spaces 2)))]
      (if ns-or-alias
        (create-function-for-alias local-zloc ns-or-alias fn-name defn-edit uri db)
        (let [form-loc (edit/to-top local-zloc)
              {form-row :row form-col :col :as form-pos} (meta (z/node form-loc))]
          [{:loc defn-edit
            :range (assoc form-pos
                          :end-row form-row
                          :end-col form-col)}
           {:loc (z/of-string "\n\n")
            :range (assoc form-pos
                          :end-row form-row
                          :end-col form-col)}])))))

(defn ^:private create-test-for-source-path
  [uri function-name-loc source-path db]
  (let [file-type (shared/uri->file-type uri)
        function-name (z/sexpr function-name-loc)
        namespace (shared/uri->namespace uri db)
        namespace-test (str namespace "-test")
        test-filename (shared/namespace+source-path->filename namespace-test source-path file-type)
        test-uri (shared/filename->uri test-filename db)
        test-namespace-file (io/file test-filename)
        max-range {:row 999999 :end-row 999999 :col 1 :end-col 1}]
    (if (shared/file-exists? test-namespace-file)
      (let [existing-text (shared/slurp-filename test-uri)
            lines (count (string/split existing-text #"\n"))
            test-text (format "(deftest %s\n  (is (= 1 1)))" (str function-name "-test"))
            test-zloc (z/up (z/of-string (str "\n" test-text)))]
        {:show-document-after-edit {:uri test-uri
                                    :take-focus? true
                                    :range max-range}
         :changes-by-uri
         {test-uri [{:loc test-zloc
                     :range {:row (inc lines) :col 1 :end-row (+ 3 lines) :end-col 1}}]}})
      (let [ns-text (format "(ns %s\n  (:require\n   [%s.test :refer [deftest is]]\n   [%s :as subject]))"
                            namespace-test
                            (if (= :cljs file-type) "cljs" "clojure")
                            namespace)
            test-text (format "(deftest %s\n  (is (= true\n         (subject/foo))))"
                              (str function-name "-test"))
            test-zloc (z/up (z/of-string (str ns-text "\n\n" test-text)))]
        {:show-document-after-edit {:uri test-uri
                                    :take-focus? true
                                    :range max-range}
         :resource-changes [{:kind "create"
                             :uri test-uri
                             :options {:overwrite? false
                                       :ignore-if-exists? true}}]
         :changes-by-uri {test-uri [{:loc test-zloc
                                     :range (-> test-zloc z/node meta)}]}}))))

(defn can-create-test? [zloc uri db]
  (when-let [function-name-loc (edit/find-var-definition-name-loc zloc (shared/uri->filename uri) db)]
    (let [source-paths (settings/get db [:source-paths])]
      (when-let [current-source-path (->> source-paths
                                          (filter #(and (string/starts-with? (shared/uri->filename uri) %)
                                                        (not (string/includes? % "test"))))
                                          first)]
        {:source-paths source-paths
         :current-source-path current-source-path
         :function-name-loc function-name-loc}))))

(defn create-test [zloc uri db]
  (when-let [{:keys [source-paths
                     current-source-path
                     function-name-loc]} (can-create-test? zloc uri db)]
    (let [test-source-paths (remove #(= current-source-path %) source-paths)]
      (cond
        (= 1 (count test-source-paths))
        (create-test-for-source-path uri function-name-loc (first test-source-paths) db)

        (< 1 (count test-source-paths))
        (let [actions (mapv #(hash-map :title %) source-paths)
              chosen-source-path (producer/window-show-message-request "Choose a source-path to create the test file" :info actions db)]
          (create-test-for-source-path uri function-name-loc chosen-source-path db))

        ;; No source paths besides current one
        :else nil))))

(defn suppress-diagnostic [zloc diagnostic-code]
  (let [form-zloc (z/up (edit/find-op zloc))
        {form-row :row form-col :col :as form-pos} (-> form-zloc z/node meta)
        loc-w-comment (z/edit-> form-zloc
                                (z/insert-left (n/uneval-node (cond-> [(n/map-node [(n/keyword-node :clj-kondo/ignore)
                                                                                    (n/spaces 1)
                                                                                    (n/vector-node [(keyword diagnostic-code)])])
                                                                       (n/newlines 1)]
                                                                (> (dec form-col) 0) (conj (n/spaces (dec form-col)))))))]
    [{:loc loc-w-comment
      :range (assoc form-pos
                    :end-row form-row
                    :end-col form-col)}]))
