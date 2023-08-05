(ns clojure-lsp.feature.format
  (:require
   [cljfmt.config :as cljfmt.config]
   [cljfmt.core :as cljfmt]
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared]
   [clojure.core.memoize :as memoize]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [medley.core :as medley]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn resolve-user-cljfmt-config [db]
  (when-let [project-root (some-> db :project-root-uri shared/uri->filename)]
    (let [config-path (settings/get db [:cljfmt-config-path] ".cljfmt.edn")
          cljfmt-config-file (if (string/starts-with? config-path "/")
                               (io/file config-path)
                               (io/file project-root config-path))]
      (medley/deep-merge
        (settings/get db [:cljfmt] {})
        (when (shared/file-exists? cljfmt-config-file)
          (if (string/ends-with? cljfmt-config-file ".clj")
            (binding [*read-eval* false]
              (read-string (slurp cljfmt-config-file)))
            (edn/read-string {:readers {'re re-pattern}} (slurp cljfmt-config-file))))))))

(defn ^:private arg-spec->cljfmt-arg [index argspec]
  (letfn [(depth [x]
            (cond
              (vector? x)      (inc (depth (first x)))
              (or (number? x)
                  (= :defn x)) 0
              :else            -1000))]
    (let [d (depth argspec)]
      (when-not (neg? d)
        [:inner d index]))))

(defn ^:private style-indent->cljfmt-spec
  "Converts Cider's `:style/indent` metadata into a cljfmt `:indents` spec.

  See the details at https://docs.cider.mx/cider/indent_spec.html but a quick sketch follows.
  - Top-level numbers or keywords are shorthand for [x].
  - The first element of the list is a number, `:defn` or `:form`.
    - Numbers give the number of special args (`[:block N]` in cljfmt)
    - `:defn` means indent like a `defn` (`[:inner 0]` in cljfmt)
    - `:form` means indent like a normal `(f a b)` form.
  - Each following value is a nested indent spec for the argument at that position.
    - cljfmt doesn't support full nesting, but it can approximate with `[:inner depth pos]`.
    - The final spec applies to all args; this corresponds to an `[:inner depth]` with no index."
  [spec]
  (let [[sym & args]        (if (vector? spec)
                              spec
                              [spec])
        sym-spec            (cond
                              (number? sym) [:block sym]
                              (= sym :defn) [:inner 0])
        arg-specs           (keep-indexed arg-spec->cljfmt-arg args)
        [tk td ti :as tail] (last arg-specs)]
    (->> (concat (when sym-spec [sym-spec])
                 (butlast arg-specs)
                 ;; The last arg spec in :style/indent applies to all remaining args.
                 ;; So if it generated a [:inner depth index], strip off the index.
                 ;; But only some args generate arg-specs, and there might be no args at all.
                 (cond
                   ;; Last arg generated [:inner depth index], remove the index
                   (and (= tk :inner)
                        (= ti (dec (count args)))) [[:inner td]]
                   ;; Last argspec doesn't match the last arg.
                   tail [tail]
                   ;; No argspecs at all.
                   :else nil))
         vec
         not-empty)))

(defn ^:private extract-style-indent-metadata [db]
  {:indents (into {}
                  (comp q/xf-analysis->var-definitions
                        (keep (fn [var-def]
                                (when-let [indent (some-> var-def
                                                          :meta
                                                          :style/indent
                                                          style-indent->cljfmt-spec)]
                                  [(if (:macro var-def)
                                     (:name var-def)
                                     (symbol (name (:ns   var-def))
                                             (name (:name var-def))))
                                   indent]))))
                  (:analysis db))})

(defn ^:private merge-configs
  "Merge two or more cljfmt configuration maps together."
  ([a b]
   (-> (merge a b)
       (assoc :indents (merge (:indents a {}) (:indents b)))
       (assoc :extra-indents (merge (:extra-indents a {}) (:extra-indents b)))))
  ([a b & more]
   (reduce merge-configs (merge-configs a b) more)))

(defn ^:private resolve-cljfmt-config [db]
  (-> cljfmt.config/default-config
      (merge-configs (extract-style-indent-metadata db))
      (merge-configs (resolve-user-cljfmt-config db))
      ;; There is a bug in cljfmt where the namespace's aliases are ignored if
      ;; :alias-map is provided. This avoids the bug in the common case where no
      ;; :alias-map is needed.
      (update :alias-map not-empty)))

(def memoize-ttl-threshold-milis 3000)

(def cljfmt-config
  (memoize/ttl resolve-cljfmt-config :ttl/threshold memoize-ttl-threshold-milis))

(defn formatting [uri {:keys [db*] :as components}]
  (if-let [text (f.file-management/force-get-document-text uri components)]
    (let [cljfmt-settings (cljfmt-config @db*)
          new-text ((cljfmt/wrap-normalize-newlines #(cljfmt/reformat-string % cljfmt-settings)) text)]
      (if (= new-text text)
        []
        [{:range shared/full-file-range
          :new-text new-text}]))
    []))

(defn range-formatting [doc-id format-pos db]
  (let [cljfmt-settings (cljfmt-config db)
        root-loc (parser/zloc-of-file db doc-id)
        start-loc (or (parser/to-pos root-loc (:row format-pos) (:col format-pos))
                      (z/leftmost* root-loc))
        start-top-loc (edit/to-top start-loc)
        end-loc (or (parser/to-pos start-top-loc (:end-row format-pos) (:end-col format-pos))
                    (z/rightmost* root-loc))
        end-top-loc (edit/to-top end-loc)

        forms (->> start-top-loc
                   (iterate z/right*) ;; maintain comments and whitespace between nodes
                   (take-while (complement z/end?))
                   (medley/take-upto #(= % end-top-loc)))
        span (merge (-> start-top-loc z/node meta (select-keys [:row :col]))
                    (-> end-top-loc z/node meta (select-keys [:end-row :end-col])))]
    [{:range (shared/->range span)
      :new-text (-> (map z/node forms)
                    n/forms-node
                    (cljfmt/reformat-form cljfmt-settings)
                    n/string)}]))
