(ns clojure-lsp.feature.hover
  (:require
   [clojure-lsp.feature.clojuredocs :as f.clojuredocs]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.string :as string]
   [taoensso.timbre :as log]))

(set! *warn-on-reflection* true)

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
                                (map #(str opening-code % closing-code))
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

(defn hover-documentation
  [{sym-ns :ns sym-name :name :keys [doc filename arglist-strs] :as _definition} db]
  (let [[content-format] (get-in @db [:client-capabilities :text-document :hover :content-format])
        arity-on-same-line? (or (settings/get db [:hover :arity-on-same-line?])
                                (settings/get db [:show-docs-arity-on-same-line?]))
        hide-filename? (settings/get db [:hover :hide-file-location?])
        join-char (if arity-on-same-line? " " "\n")
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
                     doc))
        clojuredocs (f.clojuredocs/find-hover-docs-for sym-name sym-ns db)]
    (if markdown?
      {:kind "markdown"
       :value (cond-> (str opening-code sym-line closing-code)
                clojuredocs
                (str "\n\n" (clojuredocs->hover-docs clojuredocs doc-line))

                (and (not clojuredocs)
                     doc-line)
                (str "\n\n" doc-line)

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
             (not arity-on-same-line?))
        (cons {:language "clojure"
               :value signatures})
        sym (cons {:language "clojure"
                   :value (if arity-on-same-line? sym-line sym)})))))

(defn hover [filename line column db]
  (let [analysis (:analysis @db)
        element (loop [try-column column]
                  (if-let [usage (q/find-element-under-cursor analysis filename line try-column)]
                    usage
                    (when (pos? try-column)
                      (recur (dec try-column)))))
        definition (when element (q/find-definition analysis element db))]
    (cond
      definition
      {:range (shared/->range element)
       :contents (hover-documentation definition db)}

      element
      {:range (shared/->range element)
       :contents (hover-documentation element db)}

      :else
      {:contents []})))
