(ns clojure-lsp.feature.completion-lib
  (:require
   [cheshire.core :as json]
   [clojure-lsp.http :as http]
   [clojure-lsp.logger :as logger]
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [rewrite-clj.zip :as z]
   [semver.core :as semver])
  (:import
   [clojure.lang PersistentVector]
   [java.util.zip GZIPInputStream]))

(set! *warn-on-reflection* true)

(def ^:private deps-files
  {"deps.edn" :clojure-deps
   "bb.edn" :babashka
   "project.clj" :leiningen})

(defn dep-file? [uri]
  (get deps-files (.getName (io/file uri))))

(defn lib-completion-context
  "Completion of version of libs in deps.edn, project.clj files."
  [cursor-loc uri]
  (when-let [dep-type (and cursor-loc (dep-file? uri))]
    (case dep-type
      (:clojure-deps :babashka)
      (cond
        ;; inside :mvn/version string
        (identical? :mvn/version (when (some-> cursor-loc z/prev z/tag (= :token))
                                   (some-> cursor-loc z/prev z/sexpr)))
        #:dep{:type :version
              :coordinate :mvn/version
              :lib (z/sexpr (z/prev (z/up cursor-loc)))}

        ;; inside :git/tag string
        (identical? :git/tag (when (some-> cursor-loc z/prev z/tag (= :token))
                               (some-> cursor-loc z/prev z/sexpr)))
        #:dep{:type :version
              :coordinate :git/tag
              :lib (z/sexpr (z/prev (z/up cursor-loc)))}

        ;; inside {} without coordinate
        (some-> cursor-loc z/prev z/string symbol qualified-symbol?)
        #:dep{:type :version
              :lib (some-> cursor-loc z/prev z/string symbol)}

        ;; inside {} without coordinate
        (and (some-> cursor-loc z/prev z/prev z/sexpr-able?)
             (some-> cursor-loc z/prev z/prev z/sexpr symbol?)
             (some-> cursor-loc z/prev z/prev z/string symbol qualified-symbol?))
        #:dep{:type :version
              :lib (some-> cursor-loc z/prev z/prev z/string symbol)}

        ;; lib name
        (and (some-> cursor-loc z/prev z/prev z/sexpr-able?)
             (contains? #{:deps :extra-deps :replace-deps}
                        (some-> cursor-loc z/up z/prev z/sexpr)))
        #:dep{:type :name})

      :leiningen
      (cond
        ;; lib version
        (and (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                        (some-> cursor-loc z/up z/up z/prev z/sexpr))
             (some-> cursor-loc z/sexpr string?))
        #:dep{:type :version
              :coordinate :mvn/version
              :lib (some-> cursor-loc z/leftmost z/sexpr symbol)}

        ;; lib name
        (or (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                       (some-> cursor-loc z/up z/prev z/sexpr))
            (contains? #{:dependencies :managed-dependencies :plugins :pom-plugins :extensions}
                       (some-> cursor-loc z/up z/up z/prev z/sexpr)))
        #:dep{:type :name}))))

(def initial-libs-value {:loading false :libs nil})
(defonce libs* (atom initial-libs-value))

(defn ^:private fetch-clojars-libs!
  "Return a map of libs with all its versions.
   E.g. `{foo/bar [\"0.1.0\" \"0.1.2\"]}`"
  []
  (try
    (->> "https://clojars.org/repo/all-jars.clj.gz"
         io/input-stream
         GZIPInputStream.
         io/reader
         line-seq
         (keep #(try (edn/read-string %) (catch Exception _ nil)))
         (reduce (fn [map [lib version]]
                   (if (string/ends-with? version "-SNAPSHOT")
                     map
                     (let [lib-name (if (simple-symbol? lib)
                                      (symbol (str lib) (str lib))
                                      lib)]
                       (update map lib-name conj version)))) {})
         (map (fn [[lib versions]]
                [lib (map #(hash-map :mvn/version %) (semver/sorted versions))]))
         (into {}))
    (catch Exception _
      {})))

(defn ^:private get-mvn-artifacts!
  "All the artifacts under group-id in mvn central"
  [group-id]
  (try
    (let [search-prefix "https://search.maven.org/solrsearch/select?q=g:%22"
          search-suffix "%22+AND+p:%22jar%22&core=gav&rows=100&wt=json"
          search-url (str search-prefix group-id search-suffix)
          {:keys [body status error]} (http/request! search-url)]
      (if (or error (not= 200 status))
        {}
        (->> (with-open [r (io/reader body)]
               (json/parse-stream r keyword))
             :response
             :docs
             (keep (juxt :g :a :v))
             (reduce (fn [map [group artifact version]]
                       (if (string/ends-with? version "-SNAPSHOT")
                         map
                         (update map (symbol group artifact) conj version))) {})
             (map (fn [[lib versions]]
                    [lib (map #(hash-map :mvn/version %) (semver/sorted versions))]))
             (into {}))))
    (catch Exception _
      {})))

(defn ^:private fetch-github-clojure-libs!
  []
  (try
    (->> "https://github.com/phronmophobic/dewey/releases/latest/download/deps-libs.edn.gz"
         io/input-stream
         GZIPInputStream.
         io/reader
         line-seq
         (keep #(try (edn/read-string %) (catch Exception _ nil)))
         first
         (reduce (fn [map [lib details]]
                   (assoc map lib (:versions details))) {}))
    (catch Exception _
      {})))

(defn ^:private fetch-clojure-mvn-central-libs! []
  (->> ["org.clojure" "com.cognitect"]
       (pmap get-mvn-artifacts!)
       flatten
       (reduce merge)))

(defn ^:private all-clojure-libs!
  "Fetch clojars libs, org.clojure only libs in mvn-central and github libs, caching it.
   Return a map of libs with all its versions.
   E.g. `{foo/bar [{:mvn/version \"0.1.0\"} {:git/tag \"0.1.2\" :git/sha \"123\"}]}`"
  []
  (or (:libs @libs*)
      (when-not (:loading @libs*)
        (shared/logging-task
          :internal/fetch-all-jar-libs-for-completion
          (swap! libs* assoc :loading true)
          (let [clojars-libs* (future (fetch-clojars-libs!))
                mvn-central-libs* (future (fetch-clojure-mvn-central-libs!))
                github-libs* (future (fetch-github-clojure-libs!))
                libs (shared/deep-merge {} @clojars-libs* @mvn-central-libs* @github-libs*)]
            (reset! libs* {:loading false :libs libs})
            libs)))))

(defn fetch-libs! []
  (when-not (:libs @libs*)
    (try
      (boolean (all-clojure-libs!))
      (catch Exception e
        (logger/error "Error fetching libs for completion:" e)
        false))))

(defn ^:private complete-lib-name [_lib-context cursor-value]
  (let [lib-names (keys (all-clojure-libs!))]
    (keep (fn [lib-name]
            (when (string/starts-with? (str lib-name) (str cursor-value))
              {:label (str lib-name)
               :kind :text
               :priority :lib-name})) lib-names)))

(defn ^:private complete-lib-version [{:dep/keys [coordinate lib]} matches-fn]
  (let [versions-coord (get (all-clojure-libs!) lib)
        version-fn #(or (when coordinate (get % coordinate)) (:git/tag %) (:mvn/version %))
        versions (map version-fn versions-coord)
        item-fn (fn [version label]
                  (when (or (not coordinate)
                            (matches-fn label))
                    (let [index (.indexOf ^PersistentVector (vec versions) version)]
                      {:label label
                       :detail (when (= 0 index) "latest")
                       :sort-text (format "%03d" index)
                       :kind :text
                       :priority :lib-version})))]
    (if coordinate
      (keep (fn [version] (item-fn version version)) versions)
      (keep (fn [version-coord]
              (let [version-coord-str (str version-coord)]
                (item-fn (version-fn version-coord) (subs version-coord-str 1 (dec (count version-coord-str))))))
            versions-coord))))

(defn complete [lib-context matches-fn cursor-value]
  (case (:dep/type lib-context)
    :version
    (complete-lib-version lib-context matches-fn)

    :name
    (complete-lib-name lib-context cursor-value)

    nil))
