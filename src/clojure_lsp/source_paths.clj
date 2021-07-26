(ns clojure-lsp.source-paths
  (:require
   [clojure-lsp.config :as config]
   [clojure-lsp.parser :as parser]
   [clojure-lsp.shared :as shared]
   [clojure.set :as set]
   [rewrite-clj.zip :as z]
   [taoensso.timbre :as log]))

(def default-source-paths #{"src" "test"})

(def default-lein-source-paths ["src" "src/main/clojure"])
(def default-lein-test-paths ["test" "src/test/clojure"])

(def default-bb-source-paths #{"src" "test" "script" "scripts"})

(def default-source-aliases #{:dev :test})

(defn ^:private exists?
  [^java.io.File file]
  (.exists file))

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

(defn ^:private resolve-deps-source-paths
  [{:keys [paths extra-paths aliases]}
   settings]
  (let [source-aliases (or (:source-aliases settings) default-source-aliases)
        root-source-paths (extract-source-paths paths extra-paths aliases)]
    (->> source-aliases
         (map #(get aliases % nil))
         (mapcat #(extract-source-paths (:paths %) (:extra-paths %) nil))
         (remove nil?)
         set
         (set/union root-source-paths))))

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
  (set paths))

(defn ^:private resolve-source-paths-from-files [root-path settings]
  (let [deps-file (shared/to-file root-path "deps.edn")
        lein-file (shared/to-file root-path "project.clj")
        bb-file (shared/to-file root-path "bb.edn")
        file-source-paths
        (cond-> []
          (exists? deps-file)
          (conj
            (let [deps-edn (config/read-edn-file deps-file)
                  deps-source-paths (resolve-deps-source-paths deps-edn settings)]
              (if (seq deps-source-paths)
                {:deps-source-paths deps-source-paths
                 :source-paths deps-source-paths
                 :origin :deps-edn}
                {:origin :empty-deps-edn
                 :source-paths default-source-paths})))

          (exists? lein-file)
          (conj
            (let [lein-edn (parser/lein-zloc->edn (z/of-file lein-file))
                  lein-source-paths (resolve-lein-source-paths lein-edn settings)]
              (if (seq lein-source-paths)
                {:lein-source-paths lein-source-paths
                 :source-paths lein-source-paths
                 :origin :leiningen}
                {:origin :empty-leiningen
                 :source-paths default-source-paths})))

          (exists? bb-file)
          (conj
            (let [bb-edn (config/read-edn-file bb-file)
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

(defn ^:private resolve-source-paths [root-path settings given-source-paths]
  (if given-source-paths
    {:source-paths given-source-paths
     :origins #{:settings}}
    (or (resolve-source-paths-from-files root-path settings)
        {:source-paths default-source-paths
         :origins #{:default}})))

(defn process-source-paths [root-path settings settings-source-paths]
  (let [{:keys [origins source-paths deps-source-paths lein-source-paths bb-source-paths]} (resolve-source-paths root-path settings settings-source-paths)]
    (condp set/subset? origins
      #{:settings} (log/info "Using given source-paths:" settings-source-paths)
      #{:deps-edn} (log/info "Automatically resolved source-paths from deps.edn:" deps-source-paths)
      #{:leiningen} (log/info "Automatically resolved source-paths from project.clj:" lein-source-paths)
      #{:bb} (log/info "Automatically resolved source-paths from bb.edn:" bb-source-paths)
      #{:empty-deps-edn} (log/info "Empty deps.edn source-paths, using default source-paths:" default-source-paths)
      #{:empty-leinigen} (log/info "Empty project.clj source-paths, using default source-paths:" default-source-paths)
      #{:empty-bb} (log/info "Empty bb.edn paths, using default source-paths:" default-source-paths)
      #{:default} (log/info "Using default source-paths:" default-source-paths))

    (mapv #(->> % (shared/to-file root-path) .getAbsolutePath str) source-paths)))
