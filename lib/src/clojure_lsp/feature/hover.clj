(ns clojure-lsp.feature.hover
  (:require
   [clojure-lsp.feature.clojuredocs :as f.clojuredocs]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(def line-break "\n\n----\n\n")
(def clojure-opening-code "```clojure\n")
(def java-opening-code "```java\n")
(def closing-code "\n```")

(defn ^:private drop-whitespace [n s]
  (if (> n (count s))
    s
    (let [fully-trimmed (string/triml s)
          dropped (subs s n)]
      (last (sort-by count [fully-trimmed dropped])))))

(defn ^:private count-whitespace [s]
  (- (count s) (count (string/triml s))))

(defn ^:private docstring->formatted-markdown [doc]
  (let [lines (string/split-lines doc)
        other-lines (filter (comp not string/blank?) (rest lines))
        multi-line? (> (count other-lines) 0)]
    (if-not multi-line?
      doc
      (let [indentation (apply min (map count-whitespace other-lines))
            unindented-lines (cons (first lines)
                                   (map #(drop-whitespace indentation %) (rest lines)))]
        (string/join "\n" unindented-lines)))))

(defn ^:private clojuredocs->hover-docs
  [{:keys [doc examples see-alsos notes]}
   doc-line]
  (string/join
    "\n\n"
    (cond-> []
      doc-line (conj doc-line)
      (and (not doc-line)
           doc) (conj doc)
      (seq examples) (conj "__Examples:__"
                           (->> examples
                                (map #(str clojure-opening-code % closing-code))
                                (string/join "\n---\n")))
      (seq see-alsos) (conj "__See also:__"
                            (->> see-alsos
                                 (map (fn [see-also]
                                        (let [name (name see-also)
                                              ns (namespace see-also)]
                                          (format "[%s](https://clojuredocs.org/%s/%s)"
                                                  (str ns "/" name)
                                                  ns
                                                  name))))
                                 (string/join "\n\n")))
      (seq notes) (conj "__Notes:__"
                        (->> notes
                             (map #(str %))
                             (string/join "\n---\n"))))))

(defn find-docstring
  "Find the doc string for the hovered symbol.

  If the symbol's docstring is a string literal, we can process it. If it's reaching
  into the metadata of another var to get that var's docstring (with the idiom
  `(:doc (meta #'some-var)))`, use `q/find-definition` to retrieve the `:doc` from it
  and try again.

  Limits recurring with `q/find-definition` to 3 times to avoid potential timeouts."
  [db markdown? uri doc cnt]
  (cond
    (string? doc)
    (when (seq doc)
      (if markdown?
        (docstring->formatted-markdown doc)
        doc))
    (< 2 cnt) nil
    ;; special case for `(:doc (meta #'some-var))`
    (seq? doc)
    (let [referenced-var-meta
          (and (seq? doc)
               (= :doc (first doc))
               (let [doc' (fnext doc)]
                 (and (seq? doc')
                      (= 'meta (first doc'))
                      (let [doc'' (fnext doc')]
                        (and (= 'var (first doc''))
                             (meta (second doc'')))))))
          referenced-var-docs
          (when referenced-var-meta
            (:doc (q/find-definition-from-cursor
                    db uri
                    (:row referenced-var-meta)
                    (:col referenced-var-meta))))]
      ;; Recur only when the definition has docs
      (when referenced-var-docs
        (recur db markdown? uri referenced-var-docs (inc cnt))))))

(defn ^:private hover-signatures
  [{:keys [meta arglist-strs parameters]}
   join-char]
  (or (let [node (some->> (:arglists meta) z/of-node)
            sexpr (try (z/sexpr node) (catch Exception _ nil))]
        (if (= 'quote (first sexpr))
          (string/join join-char (second sexpr))
          (z/string node)))
      (some->> arglist-strs
               (remove nil?)
               (string/join join-char))
      (some->> parameters
               (string/join ", ")
               (format "(%s)"))))

(defn hover-documentation
  [{sym-ns :ns sym-name :name :keys [doc uri return-type bucket] :as definition}
   db*
   {:keys [additional-text-edits?]}]
  (let [db @db*
        content-formats (get-in db [:client-capabilities :text-document :hover :content-format])
        arity-on-same-line? (or (settings/get db [:hover :arity-on-same-line?])
                                (settings/get db [:show-docs-arity-on-same-line?]))
        hide-filename? (settings/get db [:hover :hide-file-location?])
        additional-edits-warning-text (settings/get db [:completion :additional-edits-warning-text])
        join-char (if arity-on-same-line? " " "\n")
        signatures (hover-signatures definition join-char)
        sym (cond-> ""
              return-type (str return-type " ")
              sym-ns (str sym-ns "/")
              sym-name (str sym-name))
        sym-line (str sym (when signatures
                            (str join-char signatures)))
        markdown? (some #{"markdown"} content-formats)
        doc-line (find-docstring db markdown? uri doc 0)
        clojuredocs (or (f.clojuredocs/find-hover-docs-for sym-name sym-ns db*)
                        (when (and sym-ns (#{:cljs :cljc} (shared/uri->file-type uri)))
                          (f.clojuredocs/find-hover-docs-for
                            sym-name
                            (string/replace (name sym-ns) "cljs" "clojure")
                            db*)))
        ;; TODO Consider using URI for display purposes, especially if we
        ;; support remote LSP connections
        filename (shared/uri->filename uri)]
    (if markdown?
      {:kind "markdown"
       :value (cond-> (str (if (#{:java-member-definitions
                                  :java-class-definitions} bucket)
                             java-opening-code
                             clojure-opening-code) sym-line closing-code)
                (and additional-text-edits? additional-edits-warning-text)
                , (str "\n\n" additional-edits-warning-text)
                clojuredocs
                , (str "\n\n" (clojuredocs->hover-docs clojuredocs doc-line))
                (and (not clojuredocs)
                     doc-line)
                , (str "\n\n" doc-line)
                (and filename (not hide-filename?))
                , (str (format "%s*[%s](%s)*"
                               line-break
                               (string/replace filename #"\\" "\\\\")
                               uri)))}
      ;; Default to plaintext
      (cond-> []
        sym
        , (conj {:language "clojure"
                 :value (str (if arity-on-same-line? sym-line sym))})
        (and signatures
             (not arity-on-same-line?))
        , (conj {:language "clojure"
                 :value (str signatures)})
        (and additional-text-edits? additional-edits-warning-text)
        , (conj additional-edits-warning-text)
        doc-line
        , (conj doc-line)
        (and filename (not hide-filename?))
        , (conj filename)))))

(defn hover
  ([uri row col components] (hover uri row col components {}))
  ([uri row col {:keys [db*] :as components} docs-config]
   (let [db @db*
         cursor-element (q/find-element-under-cursor db uri row col)
         cursor-loc (some-> (f.file-management/force-get-document-text uri components)
                            parser/safe-zloc-of-string
                            (parser/to-pos row col))
         func-position (some-> cursor-loc
                               edit/find-function-usage-name-loc
                               z/node
                               meta)
         inside-ns (and cursor-loc (edit/inside-require? cursor-loc))
         element (cond
                   (or (contains? #{:var-usages :var-definitions} (:bucket cursor-element))
                       inside-ns)
                   cursor-element

                   func-position
                   (q/find-element-under-cursor db uri (:row func-position) (:col func-position))

                   :else
                   (loop [try-col col]
                     (if-let [usage (q/find-element-under-cursor db uri row try-col)]
                       usage
                       (when (pos? try-col)
                         (recur (dec try-col))))))

         definition (when element (q/find-definition db element))]
     (cond
       definition
       {:range (shared/->range element)
        :contents (hover-documentation definition db* docs-config)}

       element
       {:range (shared/->range element)
        :contents (hover-documentation element db* docs-config)}

       :else
       {:contents []}))))
