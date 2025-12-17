(ns clojure-lsp.feature.diagnostics
  (:require
   [clojure-lsp.dep-graph :as dep-graph]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.queries :as q]
   [clojure-lsp.settings :as settings]
   [clojure-lsp.shared :as shared :refer [fast=]]
   [clojure.core.async :as async]
   [clojure.string :as string])
  (:gen-class))

(set! *warn-on-reflection* true)

(def diagnostic-types-of-unnecessary-type
  #{:clojure-lsp/unused-public-var
    :redefined-var
    :redundant-do
    :redundant-expression
    :redundant-let
    :unused-binding
    :unreachable-code
    :unused-import
    :unused-namespace
    :unused-private-var
    :unused-referred-var})

(def deprecated-diagnostic-types
  #{:deprecated-var})

(defn ^:private diagnostics-start-char-coll? [lines* row col]
  (let [start-char (when-let [^String line (get @lines* (dec row))]
                     (try (.charAt line (dec col))
                          (catch StringIndexOutOfBoundsException _ nil)))]
    (contains? #{\( \[ \{} start-char)))

(defn ^:private kondo-finding->diagnostic
  [range-type
   lines*
   output-langs?
   {:keys [type message level row col end-row langs refers] :as finding}]
  (let [expression? (not= row end-row)
        simple-range? (and (not (identical? :full range-type))
                           (diagnostics-start-char-coll? lines* row col))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  (or expression? simple-range?) (assoc :end-row row :end-col col))]
    (shared/assoc-some
      {:range (shared/->range finding)
       :tags (cond-> []
               (diagnostic-types-of-unnecessary-type type) (conj 1)
               (deprecated-diagnostic-types type) (conj 2))
       :message (str message
                     (when (and output-langs?
                                (seq langs))
                       (str " [" (string/join ", " (map name langs)) "]")))
       :code (if-let [n (namespace type)]
               (str n "/" (name type))
               (name type))
       :langs langs
       :severity (shared/level->severity level)
       :source (if (identical? :clojure-lsp/unused-public-var type)
                 "clojure-lsp"
                 "clj-kondo")}
      :data (when refers
              {:refers refers}))))

(defn ^:private valid-finding? [{:keys [row col level] :as finding}]
  (when (not= level :off)
    (or (and row col)
        (logger/warn "Invalid clj-kondo finding. Cannot find position data for" finding))))

(defn ^:private exclude-ns? [uri linter db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (when-let [ns-exclude-regex-str (settings/get db [:linters linter :ns-exclude-regex])]
      (re-matches (re-pattern ns-exclude-regex-str) (str namespace)))))

(defn ^:private kondo-findings->diagnostics [uri linter db]
  (let [range-type (settings/get db [:diagnostics :range-type] :full)
        ;; we delay for performance
        lines* (delay (some-> (get-in db [:documents uri :text])
                              (string/split #"\r?\n")))
        output-langs? (some-> db :kondo-config :output :langs)]
    (when-not (exclude-ns? uri linter db)
      (->> (get-in db [:diagnostics :clj-kondo uri])
           (filter valid-finding?)
           (mapv #(kondo-finding->diagnostic range-type lines* output-langs? %))))))

(defn severity->color [severity]
  (case (int severity)
    1 :red
    2 :yellow
    3 :cyan))

(defn ^:private clj-depend-violations->diagnostics [uri level db]
  ;; TODO: it's possible, though unusual, to have several namespaces in a file.
  ;; What should we do in that case?
  (when-let [namespace (first (dep-graph/ns-names-for-uri db uri))]
    (mapv (fn [{:keys [message]}]
            (let [ns-definition (q/find-namespace-definition-by-uri db uri)]
              {:range (shared/->range ns-definition)
               :tags []
               :message message
               :code "clj-depend"
               :severity (shared/level->severity level)
               :source "clj-depend"}))
          (get-in db [:diagnostics :clj-depend (symbol namespace)]))))

(defn ^:private built-in-diagnostics [uri db]
  (get-in db [:diagnostics :built-in uri]))

(defn ^:private custom-diagnostics->diagnostics [uri db]
  (get-in db [:diagnostics :custom uri]))

(defn find-diagnostics [^String uri db]
  (let [kondo-level (settings/get db [:linters :clj-kondo :level])
        depend-level (settings/get db [:linters :clj-depend :level] :info)]
    (if (shared/jar-file? uri)
      []
      (cond-> []
        (not= :off kondo-level)
        (concat (kondo-findings->diagnostics uri :clj-kondo db))

        (not= :off depend-level)
        (concat (clj-depend-violations->diagnostics uri depend-level db))

        :always
        (concat (built-in-diagnostics uri db)
                (custom-diagnostics->diagnostics uri db))))))

(defn ^:private publish-diagnostic!* [{:keys [diagnostics-chan]} diagnostic]
  (async/>!! diagnostics-chan diagnostic))

(defn ^:private publish-all-diagnostics!* [{:keys [diagnostics-chan]} diagnostics]
  (async/<!! (async/onto-chan!! diagnostics-chan diagnostics false)))

(defn ^:private diagnostics-of-uri [uri db]
  {:uri uri
   :diagnostics (find-diagnostics uri db)})

(defn ^:private empty-diagnostics-of-uri [uri]
  {:uri uri
   :diagnostics []})

(defn publish-diagnostics! [uri {:keys [db*] :as components}]
  (publish-diagnostic!* components (diagnostics-of-uri uri @db*)))

(defn publish-all-diagnostics! [uris publish-empty? {:keys [db*] :as components}]
  (let [db @db*
        all-diagnostics (->> uris
                             (remove #(fast= :unknown (shared/uri->file-type %)))
                             (map #(diagnostics-of-uri % db)))
        diagnostics (if publish-empty?
                      all-diagnostics
                      (filter #(not-empty (:diagnostics %)) all-diagnostics))]
    (publish-all-diagnostics!* components diagnostics)))

(defn publish-empty-diagnostics! [uris components]
  (publish-all-diagnostics!* components (map empty-diagnostics-of-uri uris)))
