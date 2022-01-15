(ns clojure-lsp.settings
  (:refer-clojure :exclude [get])
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.shared :as shared]
   [clojure-lsp.source-paths :as source-paths]
   [clojure.core.memoize :as memoize]
   [clojure.string :as string]
   [clojure.walk :as walk]
   [medley.core :as medley]
   [taoensso.timbre :as log]))

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
        (update-in [:cljfmt :indents] clean-symbol-map)
        (update :document-formatting? (fnil identity true))
        (update :document-range-formatting? (fnil identity true)))))

(defn ^:private classpath-cmd->windows-safe-classpath-cmd
  [classpath]
  (if shared/windows-os?
    (into ["powershell.exe" "-NoProfile"] classpath)
    classpath))

(defn ^:private default-project-specs []
  (->> [{:project-path "project.clj"
         :classpath-cmd ["lein" "classpath"]}
        {:project-path "deps.edn"
         :classpath-cmd ["clojure" "-A:dev:test" "-Spath"]}
        {:project-path "build.boot"
         :classpath-cmd ["boot" "show" "--fake-classpath"]}
        {:project-path "shadow-cljs.edn"
         :classpath-cmd ["npx" "shadow-cljs" "classpath"]}
        {:project-path "bb.edn"
         :classpath-cmd ["bb" "print-deps" "--format" "classpath"]}]
       (map #(update % :classpath-cmd classpath-cmd->windows-safe-classpath-cmd))))

(defn ^:private setting-changed?
  [old-settings new-settings key env]
  (and (not= :unit-test env)
       (or (nil? old-settings)
           (and (clojure.core/get new-settings key)
                (not= (clojure.core/get old-settings key)
                      (clojure.core/get new-settings key))))))

(defn udpate-with-default-settings
  [old-settings new-settings project-root-uri env]
  (cond-> new-settings

    (setting-changed? old-settings new-settings :project-specs env)
    (update :project-specs #(or % (default-project-specs)))

    (setting-changed? old-settings new-settings :source-aliases env)
    (update :source-aliases #(or % source-paths/default-source-aliases))

    (setting-changed? old-settings new-settings :source-paths env)
    (update :source-paths (partial source-paths/process-source-paths (shared/uri->path project-root-uri) new-settings))))

(defn ^:private get-refreshed-settings [db]
  (let [{:keys [project-root-uri settings force-settings env]} @db
        new-project-settings (config/resolve-for-root project-root-uri)]
    (shared/deep-merge settings
                       (udpate-with-default-settings settings new-project-settings project-root-uri env)
                       force-settings)))

(def ttl-threshold-milis 1000)

(def ^:private memoized-settings
  (memoize/ttl get-refreshed-settings :ttl/threshold ttl-threshold-milis))

(defn all [db]
  (if (#{:unit-test :api-test} (:env @db))
    (get-refreshed-settings db)
    (memoized-settings db)))

(defn get
  "Memorize get settings from db.
  Re-set settings in db if reaches memoize threshold."
  ([db kws]
   (get db kws nil))
  ([db kws default]
   (let [settings (if (#{:unit-test :api-test} (:env @db))
                    (get-refreshed-settings db)
                    (memoized-settings db))]
     (get-in settings kws default))))
