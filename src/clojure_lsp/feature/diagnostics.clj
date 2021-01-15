(ns clojure-lsp.feature.diagnostics
  (:require
    [clojure-lsp.shared :as shared]
    [clj-kondo.core :as kondo]
    [clojure.string :as string]
    [clojure-lsp.db :as db]
    [clojure.core.async :as async]
    [clojure.tools.logging :as log]
    [clojure.set :as set]))

(defn ^:private diagnose-unknown-forward-declarations [usages]
  (let [forward-usages (seq (filter (fn [usage] (contains? (:tags usage) :forward))
                                    usages))
        forward-syms (set (map :sym forward-usages))
        found-declarations (set (keep (fn [usage]
                                        (when (and (contains? forward-syms (:sym usage))
                                                   (contains? (:tags usage) :declare))
                                          (:sym usage)))
                                      usages))
        unknown-forwards (filter (fn [usage]
                                   (not (contains? found-declarations (:sym usage))))
                                 forward-usages)]
    (for [usage unknown-forwards]
      {:range (shared/->range usage)
       :code :unknown
       :source "clojure-lsp"
       :message (str "Unknown forward declaration: " (:str usage))
       :severity 1})))

(defn ^:private diagnose-unused-references [uri declared-references all-envs excluded-unused-ns-declarations]
  (let [references (->> all-envs
                        (mapcat (comp val))
                        (remove (comp #(contains? % :declare) :tags))
                        (remove (comp #(contains? % :forward) :tags))
                        (map :sym)
                        set)
        unused-syms (set/difference (set (map :sym declared-references)) references)]
    (for [usage (filter (comp unused-syms :sym) declared-references)
          :let [code (condp set/subset? (:tags usage)
                       #{:param} :unused-param
                       #{:ns} :unused-ns
                       #{:public} :unused-public
                       :unused)]
          :when (and (not= code :unused-param)
                     (or (not= :unused-ns code)
                         (not-any? #(string/index-of uri %)
                                   (set/union #{"test/"} excluded-unused-ns-declarations))))]
      {:range (shared/->range usage)
       :code code
       :source "clojure-lsp"
       :message (case code
                  :unused-ns (str "Unused namespace: " (:str usage))
                  (str "Unused declaration: " (:str usage)))
       :severity 2})))

(defn usages->declarations [usages]
  (->> usages
       (filter (comp #(and (or (contains? % :declare)
                               (contains? % :refer))
                           (not (contains? % :factory))
                           (not (contains? % :unused))) :tags))
       (remove (comp #(string/starts-with? % "_") name :sym))))

(defn ^:private diagnose-unused [uri usages excluded-unused-ns-declarations]
  (let [all-envs (assoc (:file-envs @db/db) uri usages)
        declarations (usages->declarations usages)
        declared-references (remove (comp #(contains? % :alias) :tags) declarations)]
    (concat (diagnose-unused-references uri declared-references all-envs excluded-unused-ns-declarations))))

(defn ^:private kondo-finding->diagnostic [{:keys [type message level row col] :as finding}]
  (let [expression? (not= row (:end-row finding))
        finding (cond-> (merge {:end-row row :end-col col} finding)
                  expression? (assoc :end-row row :end-col col))]
    {:range (shared/->range finding)
     :message message
     :code (name type)
     :severity (case level
                 :error   1
                 :warning 2
                 :info    3)
     :source "clj-kondo"}))

(defn ^:private kondo-args [extra]
  (let [root-path (shared/uri->path (:project-root @db/db))
        user-config (get-in @db/db [:settings :clj-kondo])
        kondo-dir (.resolve root-path ".clj-kondo")]
    (cond-> {:cache true
             :cache-dir ".clj-kondo/cache"}
      (.exists (.toFile kondo-dir))
      (assoc :cache-dir (str (.resolve kondo-dir ".cache")) :config-dir (str kondo-dir))

      :always
      (merge extra)

      user-config
      (update-in [:config] merge user-config))))

(defn run-kondo-on-paths! [paths]
  (kondo/run! (kondo-args {:lint [(string/join (System/getProperty "path.separator") paths)]})))

(defn ^:private run-kondo-on-text! [text lang]
  (with-in-str text (kondo/run! (kondo-args {:lint ["-"] :lang lang}))))

(defn ^:private kondo-find-diagnostics [uri text]
  (let [file-type (shared/uri->file-type uri)
        {:keys [findings]} (run-kondo-on-text! text file-type)]
    (->> findings
         (filter #(= "<stdin>" (:filename %)))
         (map kondo-finding->diagnostic))))

(defn find-diagnostics [uri text usages excluded-unused-ns-declarations]
  (let [kondo-diagnostics (kondo-find-diagnostics uri text)
        unused (diagnose-unused uri usages excluded-unused-ns-declarations)
        unknown-forwards (diagnose-unknown-forward-declarations usages)
        result (concat unused unknown-forwards kondo-diagnostics)]
    result))

(defn notify [uri {:keys [findings]}]
  (when (seq findings)
    (async/put! db/diagnostics-chan
                {:uri uri
                 :diagnostics (->> findings
                                   (filter #(= (shared/uri->filename uri) (:filename %)))
                                   (mapv kondo-finding->diagnostic))})))
