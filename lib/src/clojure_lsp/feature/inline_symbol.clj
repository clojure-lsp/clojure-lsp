(ns clojure-lsp.feature.inline-symbol
  (:require
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(defn ^:private end-of [loc]
  (let [{:keys [end-row end-col]} (meta (z/node loc))]
    [end-row end-col]))

(defn ^:private start-of [loc]
  (let [{:keys [row col]} (meta (z/node loc))]
    [row col]))

(defn ^:private inline-data [uri row col db]
  (let [{:keys [uri bucket name-row name-col] :as definition} (q/find-definition-from-cursor db uri row col)]
    (when (or (identical? :locals bucket)
              (identical? :var-definitions bucket))
      (when-let [zloc (some-> (parser/safe-zloc-of-file db uri)
                              (parser/to-pos name-row name-col))]
        (when-let [op (some-> zloc edit/find-op z/sexpr #{'let 'def})]
          {:def-elem definition
           :def-zloc zloc
           :def-uri uri
           :def-op op})))))

(defn ^:private merge-edits [start edits]
  (apply merge-with into start edits))

(defn ^:private ref-replacements [references val-loc]
  (map (fn [{:keys [uri] :as element}]
         {uri [{:loc val-loc :range element}]})
       references))

(defn ^:private delete-between [[start-row start-col] [end-row end-col]]
  {:loc nil
   :range {:row     start-row
           :col     start-col
           :end-row end-row
           :end-col end-col}})

(defn ^:private inline-def
  "Inlines a var defined by `def`, replacing the var with its value in any file
  it's used in."
  [uri var-loc references]
  ;; inlining `foo`, defined in a `def`
  (let [def-loc (z/up var-loc)
        start (if-let [prev-loc (z/left def-loc)]
                ;; (def bar 2)| (def foo 1)
                (end-of prev-loc)
                ;; no sibling to left
                ;; |(def foo 1)
                (start-of def-loc))
        ;; (def foo 1)|
        end (end-of def-loc)]
    (-> {uri [(delete-between start end)]}
        (merge-edits (ref-replacements references (z/rightmost var-loc))))))

(defn ^:private inline-one-let-binding
  "Inlines one of several bindings in a let."
  [uri local-loc val-loc references]
  (let [start (if-let [prev-loc (z/left local-loc)]
                ;; (let [bar 2| foo 1])
                (end-of prev-loc)
                ;; no sibling to left
                ;; (let [|foo 1 bar 2])
                (start-of local-loc))
        ;; (let [foo 1| bar 2])
        end (end-of val-loc)]
    (-> {uri [(delete-between start end)]}
        (merge-edits (ref-replacements references val-loc)))))

(defn ^:private replace-refs [let-loc val-loc references]
  (reduce (fn [let-loc reference]
            (z/subedit-node
              let-loc
              #(z/replace
                 (edit/find-at-pos % (:row reference) (:col reference))
                 (z/node val-loc))))
          let-loc
          references))

(def ^:private establishes-implicit-do?
  '#{binding
     comment
     defn
     defn-
     delay
     do
     doseq
     dosync
     dotimes
     fn
     future
     io!
     let
     letfn
     locking
     loop
     sync
     try
     when
     when-first
     when-let
     when-not
     when-some
     while
     with-bindings
     with-in-str
     with-local-vars
     with-open
     with-out-str})

(defn ^:private inline-sole-let-binding
  "Inlines the last binding of a let. Splices the body into the surrounding
  context, wrapping it in a `do` if necessary."
  [uri local-loc val-loc references]
  (let [let-form-loc (z/up (z/up local-loc))
        edit-range (meta (z/node let-form-loc))
        implicit-do? (let [leftmost (z/leftmost let-form-loc)]
                       (and (not= leftmost let-form-loc)
                            (z/sexpr-able? leftmost)
                            (establishes-implicit-do? (z/sexpr leftmost))))
        let-form-loc (replace-refs let-form-loc val-loc references)
        ;; remove `let []`
        let-body (z/subedit-> let-form-loc z/down z/remove z/down z/remove)
        one-child? (-> let-body z/down z/rightmost?)]
    {uri [{:loc (if (or one-child? implicit-do?)
                  (z/of-node* (n/forms-node (n/children (z/node let-body))))
                  (z/insert-child let-body 'do))
           :range edit-range}]}))

(defn ^:private inline-let-binding [uri local-loc references]
  ;; inlining `foo`, defined in a `let`
  (let [val-loc (z/right local-loc)]
    (if (and (z/leftmost? local-loc)
             (z/rightmost? val-loc))
      (inline-sole-let-binding uri local-loc val-loc references)
      (inline-one-let-binding uri local-loc val-loc references))))

(defn inline-symbol? [uri row col db]
  (boolean (inline-data uri row col db)))

(defn inline-symbol [uri row col db]
  (when-let [{:keys [def-zloc def-uri def-op def-elem]} (inline-data uri row col db)]
    (let [references (q/find-references db def-elem false)]
      {:changes-by-uri
       (if (= def-op 'def)
         (inline-def         def-uri def-zloc references)
         (inline-let-binding def-uri def-zloc references))})))
