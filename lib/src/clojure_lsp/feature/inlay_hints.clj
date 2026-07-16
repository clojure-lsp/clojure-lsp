(ns clojure-lsp.feature.inlay-hints
  (:require
   [clojure-lsp.feature.file-management :as f.file-management]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.queries :as q]
   [clojure-lsp.refactor.edit :as edit]
   [clojure-lsp.shared :as shared :refer [fast=]]
   [clojure.string :as string]
   [edamame.core :as edamame]
   [rewrite-clj.node :as n]
   [rewrite-clj.zip :as z]))

(set! *warn-on-reflection* true)

(defn ^:private function-loc->argument-nodes [function-loc]
  (->> function-loc
       z/up
       z/node
       n/children
       (remove n/whitespace-or-comment?)
       (drop 1)))

(defn ^:private usage->function-loc [root-zloc usage]
  (when-let [function-loc (some-> root-zloc
                                  (parser/to-pos (:name-row usage) (:name-col usage))
                                  edit/find-function-usage-name-loc)]
    (let [{:keys [row col]} (meta (z/node function-loc))]
      (when (and (= row (:name-row usage))
                 (= col (:name-col usage)))
        function-loc))))

(defn ^:private arglist-str->descriptor [arglist-str]
  (try
    (let [params (vec (edamame/parse-string arglist-str {:auto-resolve #(symbol (str ":" %))}))
          rest-index (first (keep-indexed (fn [index parameter]
                                            (when (fast= '& parameter)
                                              index))
                                          params))]
      (if rest-index
        {:fixed-params (subvec params 0 rest-index)
         :rest-param (get params (inc rest-index))}
        {:fixed-params params}))
    (catch Exception _e
      nil)))

(defn ^:private matching-arglist [arglist-strs argument-count]
  (let [matches (->> arglist-strs
                     (keep arglist-str->descriptor)
                     (filter (fn [{:keys [fixed-params rest-param]}]
                               (if rest-param
                                 (>= argument-count (count fixed-params))
                                 (= argument-count (count fixed-params))))))]
    (when (= 1 (count matches))
      (first matches))))

(defn ^:private argument-index->parameter
  [{:keys [fixed-params rest-param]} argument-index]
  (if (< argument-index (count fixed-params))
    (nth fixed-params argument-index)
    rest-param))

(defn ^:private hintable-parameter? [parameter]
  (and (symbol? parameter)
       (not (fast= '_ parameter))
       (not (string/starts-with? (name parameter) "_"))))

(defn ^:private argument-matches-parameter?
  [argument-node parameter]
  (when (n/symbol-node? argument-node)
    (let [argument (n/sexpr argument-node)]
      (and (symbol? argument)
           (fast= (name parameter) (name argument))))))

(defn ^:private position-in-range?
  [{:keys [line character]} {:keys [start end]}]
  (let [position [line character]
        start-position [(:line start) (:character start)]
        end-position [(:line end) (:character end)]]
    (and (not (neg? (compare position start-position)))
         (neg? (compare position end-position)))))

(defn ^:private usage->hints [root-zloc db range usage]
  (when-let [function-loc (usage->function-loc root-zloc usage)]
    (let [argument-nodes (function-loc->argument-nodes function-loc)
          definition (q/find-definition db usage)]
      (when (and definition (not (:macro definition)))
        (when-let [arglist (matching-arglist (:arglist-strs definition) (count argument-nodes))]
          (->> argument-nodes
               (keep-indexed
                 (fn [index argument-node]
                   (let [parameter (argument-index->parameter arglist index)
                         {:keys [row col]} (meta argument-node)
                         position (shared/row-col->position row col)]
                     (when (and (hintable-parameter? parameter)
                                (not (argument-matches-parameter? argument-node parameter))
                                (position-in-range? position range))
                       {:position position
                        :label (str parameter ":")
                        :kind :parameter}))))
               vec))))))

(defn hints [uri range {:keys [db*] :as components}]
  (when-let [root-zloc (some-> (f.file-management/force-get-document-text uri components)
                               parser/safe-zloc-of-string)]
    (let [db @db*]
      (->> (get-in db [:analysis uri :var-usages])
           (mapcat (partial usage->hints root-zloc db range))
           distinct
           vec))))
