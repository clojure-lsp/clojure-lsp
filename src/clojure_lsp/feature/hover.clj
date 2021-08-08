(ns clojure-lsp.feature.hover
  (:require
   [clojure-lsp.queries :as q]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]))

(def line-break "\n\n----\n\n")
(def opening-code "```clojure\n")
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

(defn hover-documentation
  [{sym-ns :ns sym-name :name :keys [doc filename arglist-strs] :as _definition} db]
  (let [[content-format] (get-in @db [:client-capabilities :text-document :hover :content-format])
        show-docs-arity-on-same-line? (get-in @db [:settings :show-docs-arity-on-same-line?])
        hide-filename? (get-in @db [:settings :hover :hide-file-location?])
        join-char (if show-docs-arity-on-same-line? " " "\n")
        signatures (some->> arglist-strs
                            (remove nil?)
                            (string/join join-char))
        sym (cond->> sym-name
              sym-ns (str sym-ns "/"))
        sym-line (str sym (when signatures
                            (str join-char signatures)))
        markdown? (= "markdown" content-format)
        doc-line (when (seq doc)
                   (if markdown?
                     (docstring->formatted-markdown doc)
                     doc))]
    (if markdown?
      {:kind "markdown"
       :value (cond-> (str opening-code sym-line closing-code)
                doc-line (str line-break doc-line)
                (and filename (not hide-filename?))
                (str (format "%s*[%s](%s)*"
                             line-break
                             (string/replace filename #"\\" "\\\\")
                             (shared/filename->uri filename db))))}
      ;; Default to plaintext
      (cond->> []
        (and filename (not hide-filename?)) (cons filename)
        doc-line (cons doc-line)
        (and signatures
             (not show-docs-arity-on-same-line?))
        (cons {:language "clojure"
               :value signatures})
        sym (cons {:language "clojure"
                   :value (if show-docs-arity-on-same-line? sym-line sym)})))))

(defn hover [filename line column db]
  (let [analysis (:analysis @db)
        element (q/find-element-under-cursor analysis filename line column)
        definition (when element (q/find-definition analysis element))]
    (cond
      definition
      {:range (shared/->range element)
       :contents (hover-documentation definition db)}

      element
      {:range (shared/->range element)
       :contents (hover-documentation element db)}

      :else
      {:contents []})))
