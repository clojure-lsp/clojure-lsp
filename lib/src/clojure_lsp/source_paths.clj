(ns clojure-lsp.source-paths
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [lsp4clj.protocols.logger :as logger]
   [rewrite-clj.zip :as z])
  (:import
   [java.io File]))

(set! *warn-on-reflection* true)

(def default-source-paths #{"src" "test"})

(def default-lein-source-paths ["src" "src/main/clojure"])
(def default-lein-test-paths ["test" "src/test/clojure"])

(def default-bb-source-paths #{"src" "test" "script" "scripts"})

(def default-source-aliases #{:dev :test})

(defn ^:private extract-source-paths [paths extra-paths aliases]
  (->> (concat paths extra-paths)
       (map (fn [path]
              (if (keyword? path)
                (when (or (vector? (get aliases path))
                          (set? (get aliases path)))
                  (get aliases path))
                path)))
       flatten
       (remove nil?)
       set))

(defn ^:private extract-local-roots [deps]
  (when (map? deps)
    (->> (vec deps)
         (map #(some-> % second :local/root))
         (remove nil?))))

(defn ^:private deps-file->source-paths
  [deps-file settings logger]
  (let [{:keys [paths extra-paths aliases]} (config/read-edn-file deps-file logger)
        source-aliases (or (:source-aliases settings) default-source-aliases)
        root-source-paths (extract-source-paths paths extra-paths aliases)]
    (->> source-aliases
         (map #(get aliases % nil))
         (mapcat #(extract-source-paths (:paths %) (:extra-paths %) nil))
         (remove nil?)
         set
         (set/union root-source-paths))))

(defn ^:private relative-to-deps-file [file ^File deps-file]
  (if (.isAbsolute (io/file file))
    file
    (shared/normalize-file (io/file (.getParentFile deps-file) file))))

(defn deps-file->local-roots
  [deps-file settings logger]
  (let [{:keys [deps extra-deps aliases]} (config/read-edn-file deps-file logger)
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

(defn ^:private resolve-deps-source-paths
  [deps-file root-path settings logger]
  (loop [deps-files-by-root [[nil deps-file]]
         accum-source-paths nil
         recur-level 1]
    (if (>= recur-level 500)
      (logger/warn logger "Max deps source-paths resolve level found" recur-level ", maybe a cyclic dependency?")
      (let [source-paths (->> deps-files-by-root
                              (map (fn [[local-root deps-file]]
                                     (->> (deps-file->source-paths deps-file settings logger)
                                          (map #(if local-root (str (io/file local-root %)) %))
                                          set)))
                              (reduce set/union))
            local-roots (->> deps-files-by-root
                             (map second)
                             (map #(deps-file->local-roots % settings logger))
                             flatten
                             (remove nil?))
            deps-files-by-local-root (->> local-roots
                                          (map (fn [local-root]
                                                 (let [local-root (shared/relativize-filepath local-root root-path)
                                                       sub-deps-file (io/file root-path local-root "deps.edn")]
                                                   (when (shared/file-exists? sub-deps-file)
                                                     [local-root sub-deps-file]))))
                                          (remove nil?))
            final-source-paths (set/union source-paths accum-source-paths)]
        (if (seq deps-files-by-local-root)
          (recur deps-files-by-local-root final-source-paths (inc recur-level))
          final-source-paths)))))

(defn ^:private valid-path-config? [config]
  (and config
       (or (vector? config)
           (and (list? config)
                (string? (first config)))
           (set? config))))

(defn ^:private resolve-lein-source-paths
  [{:keys [source-paths test-paths profiles] :as project}
   settings]
  (when project
    (let [source-aliases (or (:source-aliases settings) default-source-aliases)
          source-paths (if (valid-path-config? source-paths)
                         source-paths
                         default-lein-source-paths)
          test-paths (if (valid-path-config? test-paths)
                       test-paths
                       default-lein-test-paths)
          root-source-paths (extract-source-paths source-paths test-paths profiles)]
      (->> source-aliases
           (map #(get profiles % nil))
           (mapcat #(extract-source-paths (:source-paths %) (:test-paths %) nil))
           (remove nil?)
           set
           (set/union root-source-paths)))))

(defn ^:private resolve-bb-source-paths
  [{:keys [paths]}]
  (set (map str paths)))

(defn ^:private manual-source-paths-calculation [root-path settings logger]
  (let [deps-file (shared/to-file root-path "deps.edn")
        lein-file (shared/to-file root-path "project.clj")
        bb-file (shared/to-file root-path "bb.edn")
        file-source-paths
        (cond-> []
          (shared/file-exists? deps-file)
          (conj
            (let [deps-source-paths (resolve-deps-source-paths deps-file (str root-path) settings logger)]
              (if (seq deps-source-paths)
                {:deps-source-paths deps-source-paths
                 :source-paths deps-source-paths
                 :origin :deps-edn}
                {:origin :empty-deps-edn
                 :source-paths default-source-paths})))

          (shared/file-exists? lein-file)
          (conj
            (let [lein-edn (parser/lein-zloc->edn (z/of-file lein-file))
                  lein-source-paths (resolve-lein-source-paths lein-edn settings)]
              (if (seq lein-source-paths)
                {:lein-source-paths lein-source-paths
                 :source-paths lein-source-paths
                 :origin :leiningen}
                {:origin :empty-leiningen
                 :source-paths default-source-paths})))

          (shared/file-exists? bb-file)
          (conj
            (let [bb-edn (config/read-edn-file bb-file logger)
                  bb-paths (resolve-bb-source-paths bb-edn)]
              (if (seq bb-paths)
                {:bb-source-paths bb-paths
                 :source-paths bb-paths
                 :origin :bb}
                {:origin :empty-bb
                 :source-paths default-bb-source-paths}))))]
    (when (seq file-source-paths)
      (reduce
        (fn [a b]
          (-> (merge a b)
              (dissoc :origin)
              (assoc :origins (set (conj (:origins a) (:origin b)))
                     :source-paths (set/union (set (:source-paths a))
                                              (set (:source-paths b))))))
        {}
        file-source-paths))))

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

(defn ^:private resolve-source-paths [root-path classpath settings given-source-paths logger]
  (if given-source-paths
    {:source-paths given-source-paths
     :origins #{:settings}}
    (or (if (get settings :use-source-paths-from-classpath true)
          (classpath->source-paths root-path classpath)
          (manual-source-paths-calculation root-path settings logger))
        {:source-paths default-source-paths
         :origins #{:default}})))

(defn process-source-paths [root-path classpath settings logger given-source-paths]
  (let [{:keys [origins source-paths classpath-paths deps-source-paths lein-source-paths bb-source-paths]} (resolve-source-paths root-path classpath settings given-source-paths logger)]
    (when (contains? origins :settings) (logger/info logger "Using given source-paths:" given-source-paths))
    (when (contains? origins :classpath) (logger/info logger "Using source-paths from classpath:" classpath-paths))
    (when (contains? origins :deps-edn) (logger/info logger "Manually resolved source-paths from deps.edn:" deps-source-paths))
    (when (contains? origins :leiningen) (logger/info logger "Manually resolved source-paths from project.clj:" lein-source-paths))
    (when (contains? origins :bb) (logger/info logger "Manually resolved source-paths from bb.edn:" bb-source-paths))
    (when (contains? origins :empty-deps-edn) (logger/info logger "Empty deps.edn source-paths, using default source-paths:" default-source-paths))
    (when (contains? origins :empty-leiningen) (logger/info logger "Empty project.clj source-paths, using default source-paths:" default-source-paths))
    (when (contains? origins :empty-bb) (logger/info logger "Empty bb.edn paths, using default source-paths:" default-source-paths))
    (when (contains? origins :default) (logger/info logger "Using default source-paths:" default-source-paths))
    (mapv #(->> % (shared/to-file root-path) .getCanonicalPath str) source-paths)))
