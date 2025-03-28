(ns clojure-lsp.source-paths
  (:require
   [babashka.fs :as fs]
   [clojure-lsp.config :as config]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(def logger-tag "[startup]")

(def default-source-paths #{"src" "test"})
(def default-source-aliases #{:dev :test})

(defn ^:private extract-local-roots [deps]
  (when (map? deps)
    (->> (vec deps)
         (map #(some-> % second :local/root))
         (remove nil?))))

(defn ^:private relative-to-deps-file [file ^File deps-file]
  (if (.isAbsolute (io/file file))
    file
    (shared/normalize-file (io/file (.getParentFile deps-file) file))))

(defn deps-file->local-roots
  [deps-file settings]
  (let [{:keys [deps extra-deps aliases]} (config/read-edn-file deps-file)
        source-aliases (or (:source-aliases settings) default-source-aliases)
        deps-local-roots (extract-local-roots deps)
        extra-deps-local-roots (extract-local-roots extra-deps)]
    (->> source-aliases
         (map #(get aliases % nil))
         (mapcat (fn [{:keys [deps extra-deps]}]
                   (concat (extract-local-roots deps)
                           (extract-local-roots extra-deps))))
         (concat deps-local-roots extra-deps-local-roots)
         (map #(relative-to-deps-file % deps-file))
         (remove nil?))))

(defn ^:private classpath->source-paths [^java.nio.file.Path root-path classpath]
  ;; Path comparisons and resolutions should always be made on
  ;; canonicalized paths, so as to be compatible across operating
  ;; systems.
  (let [root-path (fs/canonicalize root-path)
        source-paths (->> classpath
                          (remove shared/jar-file?)
                          (map (fn [path]
                                 (let [path (if (shared/absolute-path? path)
                                              (fs/path path)
                                              (fs/path root-path path))]
                                   (fs/canonicalize path))))
                          (filter (fn [^java.nio.file.Path path]
                                    (.startsWith path root-path)))
                          (mapv str))]
    (when (seq source-paths)
      {:origins #{:classpath}
       :source-paths source-paths
       :classpath-paths source-paths})))

(defn ^:private resolve-source-paths [root-path classpath given-source-paths]
  (if given-source-paths
    {:source-paths given-source-paths
     :origins #{:settings}}
    (or (classpath->source-paths root-path classpath)
        {:source-paths default-source-paths
         :origins #{:default}})))

(defn ^:private absolutize-source-paths [source-paths root-path source-paths-ignore-regex]
  (->> source-paths
       set
       (remove (fn [source-path]
                 (let [relative-source-path (shared/relativize-filepath source-path (str root-path))]
                   (some #(re-matches (re-pattern %) relative-source-path) source-paths-ignore-regex))))
       (mapv #(->> % (shared/to-file root-path) .getCanonicalPath str))))

(defn process-source-paths [settings root-path classpath given-source-paths]
  (let [source-paths-ignore-regex (get settings :source-paths-ignore-regex config/default-source-path-ignore-regexs)
        {:keys [origins source-paths]} (resolve-source-paths root-path classpath given-source-paths)
        final-source-paths (absolutize-source-paths source-paths root-path source-paths-ignore-regex)]
    (when (contains? origins :settings) (logger/info logger-tag "Using given source-paths:" final-source-paths))
    (when (contains? origins :classpath) (logger/info logger-tag "Using source-paths from classpath:" final-source-paths))
    (when (contains? origins :default) (logger/info logger-tag "Using default source-paths:" final-source-paths))
    final-source-paths))
