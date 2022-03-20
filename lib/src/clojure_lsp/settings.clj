(ns clojure-lsp.settings
  (:refer-clojure :exclude [get])
  (:require
   [clojure-lsp.config :as config]
   [clojure.core.memoize :as memoize]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [medley.core :as medley]))

(set! *warn-on-reflection* true)

(defn- typify-json [root]
  (walk/postwalk (fn [n]
                   (if (string? n)
                     (keyword n)
                     n))
                 root))

(defn- clean-symbol-map [m]
  (->> (or m {})
       (medley/map-keys #(if (string/starts-with? % "#")
                           (re-pattern (subs % 1))
                           (symbol %)))
       (medley/map-vals typify-json)))

(defn parse-source-paths [paths]
  (when (seq paths)
    (->> paths
         (keep #(when (string? %)
                  (if (string/starts-with? % ":")
                    (subs % 1)
                    %)))
         (into #{})
         (not-empty))))

(defn kwd-string [s]
  (cond
    (keyword? s) s
    (and (string? s)
         (string/starts-with? s ":")) (keyword (subs s 1))
    (string? s) (keyword s)))

(defn parse-source-aliases [aliases]
  (when (seq aliases)
    (->> aliases
         (keep kwd-string)
         (into #{})
         (not-empty))))

(defn clean-client-settings [client-settings]
  (let [kwd-keys #(medley/map-keys keyword %)]
    (-> client-settings
        (update :dependency-scheme #(or % "zipfile"))
        (update :text-document-sync-kind kwd-string)
        (update :source-paths parse-source-paths)
        (update :source-aliases parse-source-aliases)
        (update :project-specs #(->> % (mapv kwd-keys) not-empty))
        (update :cljfmt-config-path #(or % ".cljfmt.edn"))
        (update :cljfmt kwd-keys)
        (medley/update-existing-in [:cljfmt :indents] clean-symbol-map)
        (update :document-formatting? (fnil identity true))
        (update :document-range-formatting? (fnil identity true)))))

(defn ^:private get-refreshed-settings [db logger]
  (let [{:keys [project-root-uri settings force-settings]} @db
        new-project-settings (config/resolve-for-root project-root-uri logger)]
    (config/deep-merge-fixing-cljfmt settings
                                     new-project-settings
                                     force-settings)))

(def ttl-threshold-milis 1000)

(def ^:private memoized-settings
  (memoize/ttl get-refreshed-settings :ttl/threshold ttl-threshold-milis))

(defn all
  "Get memoized settings from db.
  Refreshes settings if memoize threshold met."
  [db]
  (if (or (not (:settings-auto-refresh? @db))
          (#{:unit-test :api-test} (:env @db)))
    (get-refreshed-settings db (:logger @db))
    (memoized-settings db (:logger @db))))

(defn get
  ([db kws]
   (get db kws nil))
  ([db kws default]
   (get-in (all db) kws default)))
