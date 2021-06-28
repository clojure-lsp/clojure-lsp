(ns clojure-lsp.feature.hover
  (:require
    [clojure-lsp.db :as db]
    [clojure-lsp.feature.hover :as f.hover]
    [clojure-lsp.queries :as q]
    [clojure-lsp.shared :as shared]
    [clojure.string :as string]))

(def line-break "\n----\n")
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

(defn hover-documentation [{sym-ns :ns sym-name :name :keys [doc filename arglist-strs] :as _definition}]
  (let [[content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])
        show-docs-arity-on-same-line? (get-in @db/db [:settings :show-docs-arity-on-same-line?])
        signatures (some->> arglist-strs (remove nil?) (string/join "\n"))
        sym (cond->> sym-name
              sym-ns (str sym-ns "/"))
        markdown? (= "markdown" content-format)
        sym-line (str sym (when show-docs-arity-on-same-line? (str " " signatures)))
        signatures-line (when (not show-docs-arity-on-same-line?)
                          signatures)
        doc-line (when (seq doc)
                   (if markdown?
                     (docstring->formatted-markdown doc)
                     doc))]
    (if markdown?
      {:kind "markdown"
       :value (cond-> (str opening-code sym-line closing-code)
                signatures-line (str "\n" opening-code signatures-line closing-code)
                doc-line (str line-break doc-line "\n")
                filename (str (format "%s*[%s](%s)*" line-break filename filename)))}
      ;; Default to plaintext
      [(cond-> (str sym-line "\n")
         signatures-line (str signatures-line "\n")
         doc-line (str line-break doc-line)
         filename (str line-break filename))])))

(defn hover [filename line column]
  (let [analysis (:analysis @db/db)
        element (q/find-element-under-cursor analysis filename line column)
        definition (when element (q/find-definition analysis element))]
    (cond
      definition
      {:range (shared/->range element)
       :contents (f.hover/hover-documentation definition)}

      element
      {:range (shared/->range element)
       :contents (f.hover/hover-documentation element)}

      :else
      {:contents []})))
