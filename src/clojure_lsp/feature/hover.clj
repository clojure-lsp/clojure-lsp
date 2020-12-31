(ns clojure-lsp.feature.hover
  (:require
    [clojure-lsp.db :as db]
    [clojure.string :as string]))

(def line-break "\n----\n")
(def opening-code "```clojure\n")
(def closing-code "\n```\n")

(defn ^:private drop-whitespace [n s]
  (if (> n (count s))
    s
    (let [fully-trimmed (string/triml s)
          dropped (subs s n)]
      (last (sort-by count [fully-trimmed dropped])))))

(defn ^:private count-whitespace [s]
  (- (count s) (count (string/triml s))))


(defn ^:private docstring->formatted-markdown [doc]
  (let [lines       (string/split-lines doc)
        other-lines (filter (comp not string/blank?) (rest lines))
        multi-line? (> (count other-lines) 0)]
    (str opening-code
         (if-not multi-line?
           doc
           (let [indentation      (apply min (map count-whitespace other-lines))
                 unindented-lines (cons (first lines)
                                        (map #(drop-whitespace indentation %) (rest lines)))]
             (string/join "\n" unindented-lines)))
         closing-code)))

(defn ^:private generate [content-format uri usage show-docs-arity-on-same-line?]
  (let [{:keys [sym signatures doc]} usage
        signatures (some->> signatures
                            (:strings)
                            (string/join "\n"))
        signatures (if (and show-docs-arity-on-same-line? signatures)
                     (-> signatures
                         (string/replace #"\n" ",")
                         (string/replace #"  +" " "))
                     signatures)]
    (case content-format
      "markdown" {:kind "markdown"
                  :value (cond-> (str opening-code sym " " (when show-docs-arity-on-same-line? signatures) closing-code)
                           (and (not show-docs-arity-on-same-line?) signatures) (str opening-code signatures closing-code)
                           uri (str "*" uri "*\n")
                           (seq doc) (str line-break (docstring->formatted-markdown doc)))}
      ;; Default to plaintext
      (cond-> (str sym " " (when show-docs-arity-on-same-line? signatures) "\n")
        (and (not show-docs-arity-on-same-line?) signatures) (str signatures "\n")
        uri (str uri "\n")
        (seq doc) (str line-break doc "\n")))))

(defn hover-documentation [sym-wanted file-envs]
  (let [{:keys [uri usage]} (first
                              (for [[uri usages] file-envs
                                    {:keys [sym tags] :as usage} usages
                                    :when (and (= (str sym) sym-wanted)
                                               (:declare tags))]
                                {:uri uri :usage usage}))
        [content-format] (get-in @db/db [:client-capabilities :text-document :hover :content-format])
        show-docs-arity-on-same-line? (get-in @db/db [:settings :show-docs-arity-on-same-line?])]
    (generate content-format uri usage show-docs-arity-on-same-line?)))
