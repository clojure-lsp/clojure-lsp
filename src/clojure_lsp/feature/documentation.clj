(ns clojure-lsp.feature.documentation
  (:require
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


(defn generate [content-format usage show-docs-arity-on-same-line?]
  (let [{:keys [sym signatures doc]} usage
        signatures (some->> signatures
                            (:strings)
                            (string/join "\n"))
        signatures (if (and show-docs-arity-on-same-line? signatures)
                     (-> signatures
                         (clojure.string/replace #"\n" ",")
                         (clojure.string/replace #"  +" " "))
                     signatures)]
    (case content-format
      "markdown" {:kind "markdown"
                  :value (cond-> (str opening-code sym " " (when show-docs-arity-on-same-line? signatures) closing-code)
                           (and (not show-docs-arity-on-same-line?) signatures) (str opening-code signatures closing-code)
                           (seq doc) (str line-break (docstring->formatted-markdown doc)))}
      ;; Default to plaintext
      (cond-> (str sym " " (when show-docs-arity-on-same-line? signatures) "\n")
        (and (not show-docs-arity-on-same-line?) signatures) (str signatures "\n")
        (seq doc) (str line-break doc "\n")))))
