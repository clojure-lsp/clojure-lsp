(ns clojure-lsp.source-paths
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io])
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

(defn process-source-paths [settings root-path classpath given-source-paths]
  (let [source-paths-ignore-regex (get settings :source-paths-ignore-regex ["resources.*" "target.*"])
        {:keys [origins source-paths]} (resolve-source-paths root-path classpath given-source-paths)
        final-source-paths (->> source-paths
                                set
                                (remove (fn [source-path]
                                          (let [relative-source-path (shared/relativize-filepath source-path (str root-path))]
                                            (some #(re-matches (re-pattern %) relative-source-path) source-paths-ignore-regex))))
                                (mapv #(->> % (shared/to-file root-path) .getCanonicalPath str)))]
    (when (contains? origins :settings) (logger/info startup-paths-logger-tag "Using given source-paths:" final-source-paths))
    (when (contains? origins :classpath) (logger/info startup-paths-logger-tag "Using source-paths from classpath:" final-source-paths))
    (when (contains? origins :default) (logger/info startup-paths-logger-tag "Using default source-paths:" final-source-paths))
    final-source-paths))
