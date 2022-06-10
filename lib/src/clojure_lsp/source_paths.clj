(ns clojure-lsp.source-paths
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [lsp4clj.protocols.logger :as logger])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(def startup-paths-logger-tag "[Startup]")

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
  (let [source-paths (->> classpath
                          (remove shared/jar-file?)
                          (map (comp #(.getAbsoluteFile ^File %) io/file))
                          (filter (fn [^File file]
                                    (.startsWith (.toPath file) root-path)))
                          (mapv #(.getCanonicalPath ^File %)))]
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

(defn process-source-paths [root-path classpath given-source-paths]
  (let [{:keys [origins source-paths classpath-paths]} (resolve-source-paths root-path classpath given-source-paths)]
    (when (contains? origins :settings) (logger/info startup-paths-logger-tag "Using given source-paths:" given-source-paths))
    (when (contains? origins :classpath) (logger/info startup-paths-logger-tag "Using source-paths from classpath:" classpath-paths))
    (when (contains? origins :default) (logger/info startup-paths-logger-tag "Using default source-paths:" default-source-paths))
    (mapv #(->> % (shared/to-file root-path) .getCanonicalPath str) (set source-paths))))
